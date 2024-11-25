let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let convert_to_list running_list new_item =
  if new_item = ""
  then
    []::running_list
  else
    match running_list with
    | [] -> [[new_item]]
    | a::rest -> (List.concat [a;[new_item]])::rest

let transpose a =
  let l = String.length a.(0) in
  let at = Array.make l "" in
  for i = 0 to (l - 1) do
    for j = 0 to (Array.length a) - 1 do
      at.(i) <- at.(i) ^ (Char.escaped (String.get a.(j) i))
    done
  done;
  at

let check_string_difference a b =
  let rec check_string_difference_helper a b n =
    if (String.length a) = 0 || (String.length b) = 0 then
      n
    else
      let (a_begin, a_end) = (String.get a 0, String.sub a 1 ((String.length a) - 1)) in
      let (b_begin, b_end) = (String.get b 0, String.sub b 1 ((String.length b) - 1)) in
      if a_begin = b_begin then
        check_string_difference_helper a_end b_end n
      else
        check_string_difference_helper a_end b_end (n + 1)
  in
  check_string_difference_helper a b 0

let check_pattern_symmetry_smudge a i =
  let return_value = ref true in
  let smudge_count = ref 0 in
  let l = Array.length a in
  let (start_index, stop_index) =
    if i <= l / 2 then
      (0, i - 1)
    else
      (i - (l - i), i - 1)
  in
  for k = start_index to stop_index do
    let string_diff = check_string_difference a.(k) a.(i - k + i - 1) in
    return_value := (!return_value) && (string_diff < 2);
    smudge_count := (!smudge_count) + string_diff
  done;
  if !smudge_count = 1 then
    !return_value
  else
    false

let check_pattern_symmetry a i =
  let return_value = ref true in
  let l = Array.length a in
  let (start_index, stop_index) =
    if i <= l / 2 then
      (0, i - 1)
    else
      (i - (l - i), i - 1)
  in
  for k = start_index to stop_index do
    return_value := (!return_value) && a.(k) = a.(i - k + i - 1)
  done;
  !return_value

let find_pattern_symmetry a =
  let l = Array.length a in
  let symmetry_count = ref 0 in
  for i = 1 to l - 1 do
    if check_pattern_symmetry a i then
      symmetry_count := i
  done;
  !symmetry_count

let find_pattern_symmetry_smudge a =
  let l = Array.length a in
  let symmetry_count = ref 0 in
  for i = 1 to l - 1 do
    if check_pattern_symmetry_smudge a i then
      symmetry_count := i
  done;
  !symmetry_count

let () =
  let lines = read_lines file in
  let split_pattern = List.fold_left (fun x y -> convert_to_list x y) [] lines in
  let find_summary symmetry_func =
    split_pattern
    |> List.filter (fun x -> List.length x > 0)
    |> List.rev
    |> List.map Array.of_list
    |> List.map (fun x -> (symmetry_func x, symmetry_func (transpose x)))
    |> List.map (fun (x, y) -> 100 * x + y)
    |> List.fold_left (fun x y -> x + y) 0
    |> string_of_int
    |> print_endline;
  in
  find_summary find_pattern_symmetry;
  find_summary find_pattern_symmetry_smudge;
