let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

module IntSet = Set.Make(Int)

type scratch_card = {
  (* id : int; *)
  winning_set : IntSet.t;
  my_set : IntSet.t;
}

let line_to_scratch_card s =
  let to_intset a = a
                    |> String.split_on_char ' '
                    |> List.filter (fun x -> String.length x > 0)
                    |> List.map int_of_string
                    |> IntSet.of_list in
  {
    (* id = String.sub s 5 3 |> String.trim |> int_of_string; *)
    winning_set = String.sub s 10 30 |> to_intset;
    my_set = String.sub s 42 74 |> to_intset;
  }

(* let scratch_card_to_str sc = *)
(*   "ID: " ^ (string_of_int sc.id) ^ ", Winning:" ^ (IntSet.fold (fun x s -> s ^ " " ^ (string_of_int x)) sc.winning_set "") ^ ", Mine:" ^ (IntSet.fold (fun x s -> s ^ " " ^ (string_of_int x)) sc.my_set "") *)

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let () =
  let lines = (read_lines file) in
  lines
  |> List.filter (fun x -> String.length x > 1)
  |> List.map line_to_scratch_card
  |> List.map (fun x -> IntSet.inter x.winning_set x.my_set)
  |> List.map IntSet.cardinal
  |> List.filter (fun x -> x > 0)
  |> List.map (fun x -> pow 2 (x - 1))
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int |> print_endline;
