open Base

(* Ought to use Iri_hash, but I am optimising by avoiding normalizing on equality comparison. *)
module Tbl = Hashtbl.Make(struct
  type t = iri
  let equal = Iri.equal ~normalize: false

  (* IRI has mutable state that seems to be interfering with Hashtbl.hash *)
  let clean iri = Iri.with_query iri (Iri.query iri)

  let hash iri = clean iri |> Hashtbl.hash
end)

type 'a t = 'a Tbl.t
type key = iri

let find_opt tbl iri =
  Tbl.find_opt tbl (Iri.normalize iri)

let mem tbl iri =
  Tbl.mem tbl (Iri.normalize iri)

let add tbl iri x =
  Tbl.add tbl (Iri.normalize iri) x

let replace tbl iri x =
  Tbl.replace tbl (Iri.normalize iri) x

let length = Tbl.length
let create = Tbl.create
let iter = Tbl.iter
let to_seq_values = Tbl.to_seq_values
let to_seq_keys = Tbl.to_seq_keys