let generate_pairs n =
  let rec generate_pairs_helper initial_list =
    match initial_list with
    | (0, _)::_ -> initial_list
    | (x, y)::_ -> generate_pairs_helper ((x - 1, y + 1)::initial_list)
    | [] -> []
  in
  generate_pairs_helper [(n, 0)]

let pair_to_distance = function
  | (x, y) -> y * x

let times = [38; 94; 79; 70]

let records = [241; 1549; 1074; 1091]

let times2 = [38947970]

let records2 = [241154910741091]
let n_ways times records =
  times
  |> List.map generate_pairs
  |> List.map (function x -> List.map pair_to_distance x)
  |> List.map2 (fun x y -> (x, y)) records
  |> List.map (fun (x, y) -> List.filter (fun a -> a > x) y)
  |> List.map List.length
  |> List.fold_left (fun x y -> x * y) 1
  |> string_of_int
  |> print_endline

let () =
  n_ways times records;
  n_ways times2 records2
