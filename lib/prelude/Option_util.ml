let override x y =
  match x with
  | Some _ -> x
  | None -> y
