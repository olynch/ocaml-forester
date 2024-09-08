let scheme = "forest"

let base_iri ~host =
  Iri.iri ~scheme ?host ()

let user_iri ~host str =
  Iri.iri
    ?host
    ~scheme
    ~path: (Absolute [str])
    ()

let hash_iri ~host hash_str =
  Iri.iri
    ?host
    ~scheme
    ~path: (Absolute ["hash"; hash_str])
    ()

let fresh ~host =
  Iri.iri
    ?host
    ~scheme
    ~path: (Absolute ["unstable"; string_of_int (Oo.id object end)])
    ()

let is_stable_iri iri =
  match Iri.scheme iri, Iri.path iri with
  | sch, Absolute (("unstable" | "hash") :: _) when sch = scheme -> false
  | _ -> true

let relativise_iri ~host iri =
  if Iri.scheme iri = scheme && Iri.host iri = host then
    let (Iri.Absolute components | Iri.Relative components) = Iri.path iri in
    Iri.iri ~path: (Iri.Relative components) ()
  else
    iri
