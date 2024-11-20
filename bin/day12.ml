let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

(* Adapted from Peter Norvig's solution here: *)
(* https://github.com/norvig/pytudes/blob/main/ipynb/Advent-2023.ipynb *)

let possible_damage s =
  let rec possible_damage_helper s n =
    if (String.length s) = 0 then n
    else
      let a = String.get s 0 in
      let rest = String.sub s 1 ((String.length s) - 1) in
      if a = '#' || a = '?' then
        possible_damage_helper rest (1 + n)
      else
        possible_damage_helper rest n
  in
  possible_damage_helper s 0

let rec n_arrangements s combination_tuple =
  match combination_tuple with
  | [] -> if String.contains s '#' then 0 else 1
  | r::rest ->
    if possible_damage s < (List.fold_left (fun x y -> x + y) 0 combination_tuple)
    then 0
    else
      let damaged =
        if (possible_damage (String.sub s 0 r) != r) ||
           ((String.length s) > r) && ((String.get s r) = '#') then 0
        else
        if String.length s > r then
          n_arrangements (String.sub s (r + 1) ((String.length s) - (r + 1))) rest
        else 1
      in
      let undamaged = if (String.get s 0) = '#' then 0 else
          n_arrangements (String.sub s 1 ((String.length s) - 1)) combination_tuple
      in
      damaged + undamaged

let arrangements_table = Hashtbl.create 16

let n_arrangements_cached s c =
  match Hashtbl.find_opt arrangements_table (s, c) with
  | Some x -> x
  | None ->
    let new_val = n_arrangements s c
    in
    Hashtbl.add arrangements_table (s, c) new_val;
    new_val

let () =
  let lines = read_lines file in
  let processed_lines =
    lines
    |> List.filter (fun x -> String.length x > 0)
    |> List.map (String.split_on_char ' ')
    |> List.map (fun x -> (List.hd x, List.rev x |> List.hd |> String.split_on_char ',' |> List.map int_of_string))
  in
  processed_lines
  |> List.map (fun (x, y) -> n_arrangements_cached x y)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
  processed_lines
  |> List.map (fun (x, y) -> (x ^ "?" ^ x ^ "?" ^ x ^ "?" ^ x ^ "?" ^ x, List.concat [y;y;y;y;y]))
  |> List.map (fun (x, y) -> print_endline (x); n_arrangements_cached x y)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
