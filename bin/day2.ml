let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

type revealed_set =
  {
    red : int;
    green : int;
    blue : int;
  }

(* let print_record r = *)
(*   print_endline ("R: " ^ (string_of_int r.red) ^ ", G: " ^ (string_of_int r.green) ^ ", B: " ^ (string_of_int r.blue)) *)

let construct_record r input =
  match String.split_on_char ' ' input with
  | [a; "green"] -> { r with green = int_of_string a }
  | [a; "blue"] -> { r with blue = int_of_string a }
  | [a; "red"] -> { r with red = int_of_string a }
  | _ -> r

let make_record s =
  let rec make_record_helper s r =
  match s with
  | a::rest -> make_record_helper rest (construct_record r a)
  | [] -> r
  in
  make_record_helper s { blue = 0; green = 0; red = 0}

let get_records revealed =
  revealed
  |> String.split_on_char ';'
  |> List.map String.trim
  |> List.map (String.split_on_char ',')
  |> List.map (fun x -> List.map String.trim x)
  |> List.map make_record

let parse_line line =
  match String.split_on_char ':' line with
  | game :: rest -> ((String.sub game 5 (String.length game - 5)) |> int_of_string, get_records (List.hd rest))
  | _ -> (-1, [])

let is_valid_game rlist =
  rlist
  |> List.fold_left (fun yn x -> yn && (x.red <= 12) && (x.green <= 13) && (x.blue <= 14)) true

let () =
  file
  |> read_lines
  |> List.filter (fun x -> String.length x > 0)
  |> List.map parse_line
  |> List.filter (fun x -> match x with
      | (_, b) -> is_valid_game b)
  |> List.fold_left (fun s x -> match x with
      | (i, _) -> i + s) 0
  |> string_of_int |> print_endline
  (* |> List.iter (fun x -> *)
  (*     match x with *)
  (*     | (a, b) -> print_endline (string_of_int a); List.iter print_record b) *)
