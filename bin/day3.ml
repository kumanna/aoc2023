let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let string_to_array a s =
  let l = String.length s in
  for i = 0 to l-1 do
    a.(i) <- String.get s i
  done

(* let array_to_string a = *)
(*   let s = ref "" in *)
(*   for i = 0 to Array.length a - 1 do *)
(*     s := !s ^ (Char.escaped a.(i)) *)
(*   done; *)
(*   s *)

let () =
  let a = Array.make_matrix 140 140 ' ' in
  let lines = read_lines file in
  lines
  |> List.filter (fun x -> String.length x > 0)
  |> List.iteri (fun i x -> string_to_array a.(i) x);
