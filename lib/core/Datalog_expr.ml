(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type var = string
[@@deriving show, repr]

type 'a term =
  | Const of 'a
  | Var of var
[@@deriving show, repr]

type ('sym, 'a) prop = {
  rel: 'sym;
  args: 'a term list
}
[@@deriving show, repr]

type ('sym, 'a) sequent = {
  conclusion: ('sym, 'a) prop;
  premises: ('sym, 'a) prop list
}
[@@deriving show, repr]

type ('sym, 'a) script = ('sym, 'a) sequent list
[@@deriving show, repr]

type ('sym, 'a) query = {
  var: var;
  positives: ('sym, 'a) prop list;
  negatives: ('sym, 'a) prop list
}
[@@deriving show, repr]

let map_term f = function
  | Const c -> Const (f c)
  | Var x -> Var x

let map_prop f g prop = {rel = f prop.rel; args = List.map (map_term g) prop.args}

let map_premises f g = List.map (map_prop f g)

let map_sequent f g sequent = {
  conclusion = map_prop f g sequent.conclusion;
  premises = map_premises f g sequent.premises
}

let map_script f g = List.map (map_sequent f g)

let map_query f g query = {query with
  positives = map_premises f g query.positives;
  negatives = map_premises f g query.negatives
}

let iter_script f script =
  ignore @@ map_script Fun.id f script

let iter_query f query =
  ignore @@ map_query Fun.id f query

module Constructors = struct
  let const x = Const x
  let var x = Var x
end

include Constructors

module Notation = struct
  include Constructors

  let (<<) conclusion premises = {conclusion; premises}
  let (@*) rel args = {rel; args}
end
