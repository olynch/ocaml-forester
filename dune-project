(lang dune 3.13)

(using menhir 3.0)

(name forester)

(authors "Jonathan Sterling")

(maintainers "Jonathan Sterling")

(version 4.3.1)

(generate_opam_files true)

(source
 (sourcehut jonsterling/ocaml-forester))

(bug_reports https://todo.sr.ht/~jonsterling/forester)

(homepage https://sr.ht/~jonsterling/forester/)

(license GPL-3.0-or-later)

(package
 (name forester)
 (synopsis "A tool for tending mathematical forests")
 (depends
  (menhir
   (>= 20240715))
  (ocamlgraph
   (>= 2.1.0))
  (ocaml
   (>= 5.2.0))
  dune
  ppx_deriving
  (cmdliner
   (>= 1.2.0))
  dune-build-info
  (uucp
   (>= 15.1.0))
  (eio_main
   (>= 1.1))
  (ptime
   (>= 1.1.0))
  (asai
   (>= 0.3.0))
  (yuujinchou
   (>= 5.2.0))
  (bwd
   (>= 2.3.0))
  (algaeff
   (>= 2.0.0))
  (yojson
   (>= 2.1.2))
  (toml
   (>= 7.1.0))
  (repr
   (>= 0.7.0))
  (ppx_repr
   (>= 0.7.0))
  (uri
   (>= 4.4.0))
  (pure-html
   (>= 3.5.0))
  (base64
   (>= 3.5.1))))
