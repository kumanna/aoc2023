let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let string_to_array a s =
  let l = String.length s in
  for i = 0 to l-1 do
    a.(i) <- String.get s i
  done

let array_to_string a =
  let s = ref "" in
  for i = 0 to Array.length a - 1 do
    s := !s ^ (Char.escaped a.(i))
  done;
  !s

let is_digit = function '0' .. '9' -> true | _ -> false

module MyCoords = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = compare (x1, y1) (x2, y2)
end

module MyCoordSet = Set.Make(MyCoords)

let rec break_string row_number column_number number_set_list is_number_running s =
  if String.length s < 1 then
    (* Remove invalid indices *)
    number_set_list
    |> List.map (fun (a, b) -> (a, MyCoordSet.filter (fun (x, y) -> x >= 0 && x <= 139 && y >= 0 && y <= 139) b))
  else
    let next_character = String.get s 0 in
    let search_list = [(row_number - 1, column_number - 1);
                       (row_number - 1, column_number);
                       (row_number - 1, column_number + 1);
                       (row_number, column_number - 1);
                       (row_number, column_number);
                       (row_number, column_number + 1);
                       (row_number + 1, column_number - 1);
                       (row_number + 1, column_number);
                       (row_number + 1, column_number + 1)] in
    let rest_string = String.sub s 1 (String.length s - 1) in
    let new_int = (int_of_char next_character) - (int_of_char '0') in
    if is_number_running && (is_digit next_character) then
      break_string
        row_number
        (column_number + 1)
        (match number_set_list with
         | [] -> []
         | (a, b)::rest -> (10 * a + new_int,
                     (MyCoordSet.union b)
                       (MyCoordSet.of_list search_list))::rest)
        true rest_string
    else if is_number_running && not (is_digit next_character) then
      break_string row_number (column_number + 1) number_set_list false rest_string
    else if (not is_number_running) && (is_digit next_character) then
      break_string row_number (column_number + 1)
        ((new_int, MyCoordSet.of_list search_list)::number_set_list) true rest_string
    else if (not is_number_running) && not (is_digit next_character) then
      break_string row_number (column_number + 1) number_set_list false rest_string
    else []

let check_for_character a indices =
  List.fold_left (fun running_truth (i, j) -> running_truth ||
                                              (not (is_digit a.(i).(j)) &&
                                               not (a.(i).(j) = '.'))) false indices

let get_valid_integers a number_set_list =
  number_set_list
  |> List.filter (fun (_, indices) -> check_for_character a (MyCoordSet.to_list indices))

let () =
  let a = Array.make_matrix 140 140 ' ' in
  let b = Array.make 140 "" in
  let lines = read_lines file in
  lines
  |> List.filter (fun x -> String.length x > 0)
  |> List.iteri (fun i x -> string_to_array a.(i) x);
  Array.iteri (fun i x -> b.(i) <- array_to_string x) a;
  b
  |> Array.to_list
  |> List.mapi (fun i x -> break_string i 0 [] false x)
  |> List.concat
  |> get_valid_integers a
  |> List.map (fun (x, _) -> x)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int |> print_endline
