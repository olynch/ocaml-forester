open Query

open Locally_nameless(Global_name)

module Dx = Datalog_expr

let context_datalog vtx : _ Dx.query =
  let open Dx.Notation in
  let x = "X" in
  Dx.{
    var = x;
    positives = [Builtin_relation.transclusion @* [var x; const vtx]];
    negatives = []
  }

let backlinks_datalog vtx : _ Dx.query =
  let open Dx.Notation in
  let x = "X" in
  Dx.{
    var = x;
    positives = [Builtin_relation.links @* [var x; const vtx]];
    negatives = []
  }

let related_datalog vtx : _ Dx.query =
  let open Dx.Notation in
  let x = "X" in
  Dx.{
    var = x;
    positives = [Builtin_relation.links @* [const vtx; var x]];
    negatives = [Builtin_relation.is_reference @* [var x]]
  }

let contributions_datalog vtx : _ Dx.query =
  let open Dx.Notation in
  let x = "X" in
  Dx.{
    var = x;
    positives = [Builtin_relation.contribution @* [var x; const vtx]];
    negatives = []
  }

let references_datalog vtx : _ Datalog_expr.query =
  let open Dx.Notation in
  let x = "X" in
  Dx.{
    var = x;
    positives = [Builtin_relation.references @* [const vtx; var x]];
    negatives = []
  }
