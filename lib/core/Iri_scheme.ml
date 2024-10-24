open Forester_prelude

let scheme = "forest"

let base_iri ~host =
  Iri.iri ~scheme ~host ()

let user_iri ~host str =
  Iri.iri
    ~host
    ~scheme
    ~path: (Absolute [str])
    ()

let hash_iri ~host hash_str =
  Iri.iri
    ~host
    ~scheme
    ~path: (Absolute ["hash"; hash_str])
    ()

let fresh ~host =
  Iri.iri
    ~host
    ~scheme
    ~path: (Absolute ["unstable"; string_of_int (Oo.id object end)])
    ()

let is_named_iri iri =
  match Iri.scheme iri, Iri.path iri with
  | sch, Absolute (("unstable" | "hash") :: _) when sch = scheme -> false
  | _ -> true

let relativise_iri ~host iri =
  if Iri.scheme iri = scheme && Iri.host iri = Some host then
    let (Iri.Absolute components | Iri.Relative components) = Iri.path iri in
    Iri.iri ~path: (Iri.Relative components) ()
  else
    iri

let () =
  let@ exn = Printexc.register_printer in
  match exn with
  | Iri.Error err ->
    Option.some @@ Format.sprintf "Iri.error (%s)" (Iri.string_of_error err)
  | _ -> None
