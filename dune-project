(lang dune 3.13)

;;; SPDX-FileCopyrightText: 2024 The Forester Project Contributors
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(using menhir 3.0)

(name forester)

(authors "Jonathan Sterling")

(maintainers "Jonathan Sterling")

(version 5.0)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (sourcehut jonsterling/ocaml-forester))

(bug_reports https://todo.sr.ht/~jonsterling/forester)

(homepage https://sr.ht/~jonsterling/forester/)

(license GPL-3.0-or-later)

(cram enable)

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
  (iri
   (>= 1.0.0))
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
   (>= 3.9.5))
  (base64
   (>= 3.5.1))
  (datalog
   (>= 0.7))
  (cid
   (>= 0.1.0))
  (lsp
   (>= 0.19.0))
  (jsonrpc
   (>= 1.22.0))
  (ppx_yojson_conv
   (>= 0.17.0))
  (logs
   (>= 0.7.0))
  (bisect_ppx
   (>= 2.8.3))
  (cohttp-eio
   (>= 6.0.0))
  (odoc
   (>= 2.4.4))
  (brr
   (>= 0.0.7))
  (note
   (>= 0.0.3))
  (alcotest :with-test)))
