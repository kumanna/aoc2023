let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let rec successive_diff l =
  match l with
  | [] -> []
  | [_] -> []
  | a::b::rest -> (b - a)::(successive_diff (b::rest))

let rec find_zero_diff l running_lists =
  if List.fold_left (fun x y -> x && (y = 0)) true l then running_lists
  else find_zero_diff (successive_diff l) (l::running_lists)

let find_next_number l =
  find_zero_diff l []
  |> List.map List.rev
  |> List.map List.hd
  |> List.fold_left (fun x y -> x + y) 0

let () =
  file
  |> read_lines
  |> List.filter (fun x -> String.length x > 1)
  |> List.map (String.split_on_char ' ')
  |> List.map (fun x -> List.map int_of_string x)
  |> List.map find_next_number
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline
