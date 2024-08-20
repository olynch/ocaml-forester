let nub xs =
  let rec loop acc =
    function
    | [] -> List.rev acc
    | x :: xs ->
      let acc = if List.mem x acc then acc else x::acc in
      loop acc xs
  in
  loop [] xs