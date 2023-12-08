open Core

type target =
  | Xml
  | Rss

module type Handler =
sig
  val route : target -> addr -> string
  val source_path : addr -> string option
  val is_root : addr -> bool
  val backlinks : addr -> Sem.tree list
  val related : addr -> Sem.tree list
  val bibliography : addr -> Sem.tree list
  val parents : addr -> Sem.tree list
  val children : addr -> Sem.tree list
  val contributors : addr -> string list
  val contributions : addr -> Sem.tree list
  val enqueue_latex : name:string -> packages:string list -> source:string -> unit
  val get_doc : addr -> Sem.tree option
  val run_query : Sem.t Query.t -> Sem.tree list
end

module Perform : Handler

module Run (_ : Handler) :
sig
  val run : (unit -> 'a) -> 'a
end
