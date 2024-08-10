open Forester_core

module Xmlns_map =
struct
  type t =
    {prefix_to_xmlns : string String_map.t;
     xmlns_to_prefixes : string list String_map.t}

  let empty =
    {prefix_to_xmlns = String_map.empty;
     xmlns_to_prefixes = String_map.empty}

  let assoc ~prefix ~xmlns env =
    {prefix_to_xmlns = String_map.add prefix xmlns env.prefix_to_xmlns;
     xmlns_to_prefixes = String_map.add_to_list xmlns prefix env.xmlns_to_prefixes}
end


module Make_writer (Elt : sig type t end) =
struct
  type _ Effect.t += Yield : Elt.t -> unit Effect.t

  let yield x = Effect.perform (Yield x)

  let run f =
    let open Effect.Deep in
    try_with (fun () -> let r = f () in [], r) ()
      { effc =
          fun (type a) (eff : a Effect.t) ->
            match eff with
            | Yield x -> Option.some @@ fun (k : (a, _) continuation) ->
              let xs, r = continue k () in
              x::xs, r
            | _ -> None }

  let register_printer f = Printexc.register_printer @@ function
    | Effect.Unhandled (Yield elt) -> f (`Yield elt)
    | _ -> None

  let () = register_printer @@ fun _ -> Some "Unhandled effect; use Make_writer.run"
end

module Make () =
struct
  type xmlns_attr = {prefix: string; xmlns: string}

  module E = Algaeff.State.Make (Xmlns_map)
  module Decls = Make_writer (struct type t = xmlns_attr end)

  let find_xmlns_for_prefix prefix =
    let env = E.get ()  in
    String_map.find_opt prefix env.prefix_to_xmlns

  let rec normalise_qname (qname : xml_qname) =
    let scope = E.get () in
    match qname.xmlns with
    | None ->
      begin
        match String_map.find_opt qname.prefix scope.prefix_to_xmlns with
        | None -> qname
        | Some xmlns -> {qname with xmlns = Some xmlns}
      end

    | Some xmlns ->
      begin
        match
          String_map.find_opt qname.prefix scope.prefix_to_xmlns,
          String_map.find_opt xmlns scope.xmlns_to_prefixes
        with
        | None, (None | Some []) ->
          E.modify (Xmlns_map.assoc ~prefix:qname.prefix ~xmlns);
          Decls.yield {prefix = qname.prefix; xmlns};
          qname
        | Some xmlns', Some prefixes ->
          if xmlns' = xmlns && List.mem qname.prefix prefixes then
            qname
          else
            normalise_qname {qname with prefix = qname.prefix ^ "_"}
        | _, Some (prefix' :: _) ->
          {qname with prefix = prefix'}
        | Some xmlns', None ->
          normalise_qname {qname with prefix = qname.prefix ^ "_"}
      end

  let within_scope kont =
    let old_scope = E.get () in
    let added, r =
      Decls.run @@ fun () ->
      kont ()
    in
    E.set old_scope;
    added, r

  let run ~reserved kont =
    let init =
      let alg env {prefix; xmlns} =
        Xmlns_map.assoc ~prefix ~xmlns env
      in
      List.fold_left alg Xmlns_map.empty reserved
    in
    E.run ~init kont
end