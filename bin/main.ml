let file = "/tmp/input.txt"

(* (\* read the entire file *\) *)
(* let read_file file = *)
(*   In_channel.with_open_bin file In_channel.input_all *)

(* read lines *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

(* let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false *)
let is_digit = function '0' .. '9' -> true | _ -> false

let explode s = List.init (String.length s) (String.get s)
let rec find_first_integer s =
  match s with
  | a :: _ when is_digit a -> int_of_char a - int_of_char '0'
  | _::rest -> find_first_integer rest
  | _ -> 0

let find_last_integer s =
  s |> List.rev |> find_first_integer

let rec rebuild_string s1 s2 =
  if String.length s2 == 0 then s1 else
    let new_s1 = s1 ^ String.sub s2 0 1 in
    if (String.ends_with ~suffix:"1" new_s1 ||
       String.ends_with ~suffix:"2" new_s1 ||
       String.ends_with ~suffix:"3" new_s1 ||
       String.ends_with ~suffix:"4" new_s1 ||
       String.ends_with ~suffix:"5" new_s1 ||
       String.ends_with ~suffix:"6" new_s1 ||
       String.ends_with ~suffix:"7" new_s1 ||
       String.ends_with ~suffix:"8" new_s1 ||
       String.ends_with ~suffix:"9" new_s1) then
      (new_s1 ^ (String.sub s2 1 (String.length s2 - 1)))
    else if String.ends_with ~suffix:"one" new_s1 then
      (String.sub new_s1 0 ((String.length new_s1) - (String.length "one")) ^ "1") ^ (String.sub s2 1 (String.length s2 - 1))
    else if String.ends_with ~suffix:"two" new_s1 then
      (String.sub new_s1 0 ((String.length new_s1) - (String.length "two")) ^ "2") ^ (String.sub s2 1 (String.length s2 - 1))
    else if String.ends_with ~suffix:"three" new_s1 then
      (String.sub new_s1 0 ((String.length new_s1) - (String.length "three")) ^ "3") ^ (String.sub s2 1 (String.length s2 - 1))
    else if String.ends_with ~suffix:"four" new_s1 then
      (String.sub new_s1 0 ((String.length new_s1) - (String.length "four")) ^ "4") ^ (String.sub s2 1 (String.length s2 - 1))
    else if String.ends_with ~suffix:"five" new_s1 then
      (String.sub new_s1 0 ((String.length new_s1) - (String.length "five")) ^ "5") ^ (String.sub s2 1 (String.length s2 - 1))
    else if String.ends_with ~suffix:"six" new_s1 then
      (String.sub new_s1 0 ((String.length new_s1) - (String.length "six")) ^ "6") ^ (String.sub s2 1 (String.length s2 - 1))
    else if String.ends_with ~suffix:"seven" new_s1 then
      (String.sub new_s1 0 ((String.length new_s1) - (String.length "seven")) ^ "7") ^ (String.sub s2 1 (String.length s2 - 1))
    else if String.ends_with ~suffix:"eight" new_s1 then
      (String.sub new_s1 0 ((String.length new_s1) - (String.length "eight")) ^ "8") ^ (String.sub s2 1 (String.length s2 - 1))
    else if String.ends_with ~suffix:"nine" new_s1 then
      (String.sub new_s1 0 ((String.length new_s1) - (String.length "nine")) ^ "9") ^ (String.sub s2 1 (String.length s2 - 1))
    else
      rebuild_string new_s1 (String.sub s2 1 (String.length s2 - 1))

let rec rebuild_string_rev s1 s2 =
  if String.length s1 == 0 then s2 else
    let new_s1 = (String.sub s1 0 (String.length s1 - 1)) in
    let new_s2 = (String.sub s1 (String.length s1 - 1)) 1 ^ s2 in
    if (String.starts_with ~prefix:"1" new_s2 ||
       String.starts_with ~prefix:"2" new_s2 ||
       String.starts_with ~prefix:"3" new_s2 ||
       String.starts_with ~prefix:"4" new_s2 ||
       String.starts_with ~prefix:"5" new_s2 ||
       String.starts_with ~prefix:"6" new_s2 ||
       String.starts_with ~prefix:"7" new_s2 ||
       String.starts_with ~prefix:"8" new_s2 ||
       String.starts_with ~prefix:"9" new_s2) then
      (new_s1 ^ new_s2)
    else if String.starts_with ~prefix:"one" new_s2 then
      new_s1 ^ "1" ^ (String.sub new_s2 (String.length "one") ((String.length new_s2) - (String.length "one")))
    else if String.starts_with ~prefix:"two" new_s2 then
      new_s1 ^ "2" ^ (String.sub new_s2 (String.length "two") ((String.length new_s2) - (String.length "two")))
    else if String.starts_with ~prefix:"three" new_s2 then
      new_s1 ^ "3" ^ (String.sub new_s2 (String.length "three") ((String.length new_s2) - (String.length "three")))
    else if String.starts_with ~prefix:"four" new_s2 then
      new_s1 ^ "4" ^ (String.sub new_s2 (String.length "four") ((String.length new_s2) - (String.length "four")))
    else if String.starts_with ~prefix:"five" new_s2 then
      new_s1 ^ "5" ^ (String.sub new_s2 (String.length "five") ((String.length new_s2) - (String.length "five")))
    else if String.starts_with ~prefix:"six" new_s2 then
      new_s1 ^ "6" ^ (String.sub new_s2 (String.length "six") ((String.length new_s2) - (String.length "six")))
    else if String.starts_with ~prefix:"seven" new_s2 then
      new_s1 ^ "7" ^ (String.sub new_s2 (String.length "seven") ((String.length new_s2) - (String.length "seven")))
    else if String.starts_with ~prefix:"eight" new_s2 then
      new_s1 ^ "8" ^ (String.sub new_s2 (String.length "eight") ((String.length new_s2) - (String.length "eight")))
    else if String.starts_with ~prefix:"nine" new_s2 then
      new_s1 ^ "9" ^ (String.sub new_s2 (String.length "nine") ((String.length new_s2) - (String.length "nine")))
    else
      rebuild_string_rev new_s1 new_s2

let () =
  file
  |> read_lines
  |> List.filter (fun x -> (String.length x > 1))
  |> List.map (fun x -> rebuild_string "" x)
  |> List.map (fun x -> rebuild_string_rev x "")
  |> List.map (fun x -> (x, explode x))
  |> List.map (fun x ->
      match x with
      | (a, b) -> (a, (find_first_integer b, find_last_integer b)))
  |> List.map (fun x -> match x with
    | (s, (a, b)) -> print_endline (s ^ ": " ^ (string_of_int a) ^ (string_of_int b));
      x)
  |> List.map (fun x ->
      match x with
      | (_, (a, b)) -> ((string_of_int a) ^ (string_of_int b)) |> int_of_string)
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int |> print_endline
