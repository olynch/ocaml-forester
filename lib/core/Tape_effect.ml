module type S = sig
  val run : tape:Syn.t -> (unit -> 'a) -> 'a
  val pop_node_opt : unit -> Syn.node Range.located option

  val pop_arg_opt : unit -> Syn.t Range.located option
  val pop_arg : loc:Range.t option -> Syn.t Range.located
  val pop_args : unit -> Syn.t Range.located list
end

module Make () = struct
  open Bwd

  module T = Algaeff.State.Make (struct type t = Syn.t end)

  let pop_node_opt () =
    match T.get () with
    | node :: nodes ->
      T.set nodes;
      Some node
    | [] -> None

  let push_node node =
    T.modify @@ fun nodes -> node :: nodes

  let pop_arg_opt () =
    match T.get () with
    | Range.{value = Syn.Group (Braces, arg); _} as node :: nodes ->
      T.set nodes;
      Some ({node with value = arg})
    | Range.{value = (Syn.Sym _ | Syn.Verbatim _ | Syn.Var _); _} as node :: nodes ->
      T.set nodes;
      Some ({node with value = [node]})
    | _ -> None

  let pop_arg ~loc =
    match pop_arg_opt () with
    | Some arg -> arg
    | None -> Reporter.fatalf ?loc Type_error "Expected argument"

  let pop_args () =
    let rec loop acc =
      match pop_arg_opt () with
      | Some arg -> loop @@ Bwd.Snoc (acc, arg)
      | None -> Bwd.prepend acc []
    in
    loop Bwd.Emp

  let run ~tape =
    T.run ~init:tape

end
