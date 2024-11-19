let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let get_question_indices s =
  let rec get_question_indices_helper s n indices =
    if String.length s < 1 then indices
    else
      let current_char = String.get s 0 in
      let rest_str = String.sub s 1 ((String.length s) - 1) in
      if current_char = '?' then
        get_question_indices_helper rest_str (n + 1) (n::indices)
      else
        get_question_indices_helper rest_str (n + 1) indices
  in
  get_question_indices_helper s 0 [] |> List.rev

let eh_hashtable = Hashtbl.create 16

let extract_hash_strings s =
  let rec extract_hash_strings_helper s previous_hash running_list =
    if String.length s < 1 then running_list
    else
      let current_char = String.get s 0 in
      let rest_str = String.sub s 1 ((String.length s) - 1) in
      match running_list with
      | [] -> if current_char = '#' then extract_hash_strings_helper rest_str true [1]
        else extract_hash_strings_helper rest_str false []
      | a::r ->
        if current_char = '#' && previous_hash then
          extract_hash_strings_helper rest_str true ((a + 1)::r)
        else if current_char = '#' && not previous_hash then
          extract_hash_strings_helper rest_str true (1::running_list)
        else if current_char != '#' && previous_hash then
          extract_hash_strings_helper rest_str false running_list
        else extract_hash_strings_helper rest_str false running_list
  in
  let retval =
    try Hashtbl.find eh_hashtable s
    with Not_found ->
      (extract_hash_strings_helper s false []) |> List.rev
  in
  retval


let cartesian_product l1 l2 =
  l1 |> List.map (fun x -> (x, l2)) |> List.map (fun (x, y) -> (List.map (fun z -> List.concat [x; z]) y)) |> List.concat

let cp_hashtable = Hashtbl.create 16

let rec nth_cartesian_product n l =
  let retval =
    try Hashtbl.find cp_hashtable (n, l)
    with Not_found ->
      if n = 0 then []
      else if n = 1 then
        [["#"];["."]]
      else if n = 2 then cartesian_product [["#"];["."]] l
      else nth_cartesian_product (n - 1) (cartesian_product [["#"];["."]] l)
  in
  retval

let q_hashtable = Hashtbl.create 16

let rec build_unquestioned_string s fill_vals =
  let retval =
    try Hashtbl.find q_hashtable (s, fill_vals)
    with Not_found ->
    match String.index_opt s '?' with
    | Some n -> build_unquestioned_string ((String.sub s 0 n) ^ (List.hd fill_vals) ^ (String.sub s (n + 1) ((String.length s) - n - 1))) (List.tl fill_vals)
    | None -> s
  in
  retval

let () =
  let lines = read_lines file in
  lines
  |> List.filter (fun x -> String.length x > 0)
  |> List.map (String.split_on_char ' ')
  |> List.map (fun x -> (List.hd x, List.rev x |> List.hd |> String.split_on_char ',' |> List.map int_of_string))
  |> List.mapi (fun i (x, y) -> print_endline ("N: " ^ (string_of_int i));
      (nth_cartesian_product (1 + (get_question_indices x |> List.length)) [["#";"."]]
       |> List.map (fun a -> build_unquestioned_string x a), y))
  |> List.map (fun (x, y) -> (List.filter (fun a -> (extract_hash_strings a) = y) x) |> List.length)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline
