open Base

module type S = sig
  val register_addr : addr -> unit
  val get_all_addrs : unit -> Addr_set.t
  val get : Query.mode -> Query.Rel.t -> Addr_graph.t
  val add_edge : Query.Rel.t -> source:addr -> target:addr -> unit
end


module Make () =
struct
  let all_addrs_ref : Addr_set.t ref =
    ref Addr_set.empty

  let get_all_addrs () = !all_addrs_ref

  let rel_to_graph : (Query.Rel.t, Addr_graph.t) Hashtbl.t =
    Hashtbl.create 20

  let rel_to_preorder : (Query.Rel.t, Addr_graph.t) Hashtbl.t =
    Hashtbl.create 20

  let get_graph rel =
    match Hashtbl.find_opt rel_to_graph rel with
    | None ->
      let gph = Addr_graph.create () in
      Hashtbl.add rel_to_graph rel gph;
      gph
    | Some gph -> gph

  let get_preorder rel =
    match Hashtbl.find_opt rel_to_preorder rel with
    | None ->
      let message = Format.asprintf "Computing reflexive-transitive closure of %s" rel in
      Reporter.profile message @@ fun () ->
      let gph = Addr_graph.transitive_closure ~reflexive:true @@ get_graph rel in
      Hashtbl.add rel_to_preorder rel gph;
      gph
    | Some gph -> gph

  let get (mode : Query.mode) =
    match mode with
    | Edges -> get_graph
    | Paths -> get_preorder

  let register_addr addr =
    Hashtbl.clear rel_to_preorder;
    all_addrs_ref := Addr_set.add addr !all_addrs_ref;
    rel_to_graph |> Hashtbl.iter @@ fun _ gph ->
    Addr_graph.add_vertex gph addr

  let add_edge rel ~source ~target =
    Hashtbl.remove rel_to_preorder rel;
    let gph = get_graph rel in
    Addr_graph.add_edge gph source target
end
