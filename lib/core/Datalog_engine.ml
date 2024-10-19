module T = Xml_tree
module D = Datalog.Default
(* BottomUp.Make(struct include Vertex let to_string = show end) *)

type relation = string
type vertex = Vertex.t
type term = D.term
type literal = D.literal
type clause = D.clause
type var = int

let symbol_of_string str = D.StringSymbol.make str

let vertex_tbl = Hashtbl.create 1000

let pack_vertex vtx : D.symbol =
  let encoding = symbol_of_string @@ string_of_int @@ Vertex.hash vtx in
  begin
    match Hashtbl.find_opt vertex_tbl encoding with
    | Some _ -> ()
    | None ->
      Hashtbl.add vertex_tbl encoding vtx
  end;
  encoding

let unpack_vertex str = Hashtbl.find_opt vertex_tbl str

let var_of_string = String.hash

let mk_var = D.mk_var
let mk_const vtx = D.mk_const (pack_vertex vtx)
let mk_literal rel = D.mk_literal (symbol_of_string rel)
let mk_clause = D.mk_clause

let vertex_of_term = function
  | D.Const sym ->
    begin
      match unpack_vertex sym with
      | Some vtx -> vtx
      | None -> failwith "vertex_of_term: unpack_vertex failed"
    end
  | _ -> failwith "const_of_term: unexpected variable"

type db = D.db
let db_create = D.db_create
let db_add_fact db lit = D.db_add_fact db lit
let db_add db clause = D.db_add db clause

type answers = D.Query.set

let list_of_answers answers =
  List.map (Array.map vertex_of_term) @@
    D.Query.to_list answers

let ask = D.Query.ask
