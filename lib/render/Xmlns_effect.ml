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

module Make () =
struct
  module Xmlns_prefixes = Algaeff.Reader.Make (Xmlns_map)

  let find_xmlns_for_prefix prefix =
    let env = Xmlns_prefixes.read ()  in
    String_map.find_opt prefix env.prefix_to_xmlns

  type xmlns_prefix = {prefix: string; xmlns: string}
  type 'b kont = added:xmlns_prefix option -> prefix:string -> 'b

  let rec with_normalised_prefix ~prefix ~xmlns (kont : _ kont) =
    match xmlns with
    | Some xmlns ->
      begin
        let open Xmlns_map in
        let env = Xmlns_prefixes.read () in
        let exception Shadowing in
        try
          begin
            match
              String_map.find_opt prefix env.prefix_to_xmlns,
              String_map.find_opt xmlns env.xmlns_to_prefixes
            with
            | None, (None | Some []) ->
              let env = assoc ~prefix ~xmlns env in
              Xmlns_prefixes.run ~env @@ fun () ->
              kont ~added:(Some {prefix;xmlns}) ~prefix
            | Some xmlns', Some prefixes ->
              if xmlns' = xmlns && List.mem prefix prefixes then
                kont ~added:None ~prefix
              else
                raise Shadowing
            | _, Some (prefix' :: _) ->
              kont ~added:None ~prefix:prefix'
            | Some xmlns', None ->
              raise Shadowing
          end
        with Shadowing ->
          with_normalised_prefix ~prefix:(prefix ^ "_") ~xmlns:(Some xmlns) kont
      end
    | _ ->
      kont ~added:None ~prefix


  let run ~reserved k =
    let env =
      let alg env {prefix; xmlns} =
        Xmlns_map.assoc ~prefix ~xmlns env
      in
      List.fold_left alg Xmlns_map.empty reserved
    in
    Xmlns_prefixes.run ~env:Xmlns_map.empty k
end