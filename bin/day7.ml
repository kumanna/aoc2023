module CamelCard = struct
  type t = {
    point_tuple : int * int * int * int * int * int;
    bid : int;
  }

  let char_to_score = function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | '9' -> 9
    | '8' -> 8
    | '7' -> 7
    | '6' -> 6
    | '5' -> 5
    | '4' -> 4
    | '3' -> 3
    | '2' -> 2
    | 'X' -> 0
    | _ -> failwith "Invalid card!"

  let create_hand s =
    let card1 = String.get s 0 in
    let card2 = String.get s 1 in
    let card3 = String.get s 2 in
    let card4 = String.get s 3 in
    let card5 = String.get s 4 in
    let bid = (String.split_on_char ' ' s) |> List.rev |> List.hd |> int_of_string
    in
    let hand_point =
      match List.sort compare [card1;card2;card3;card4;card5] with
      | [a;b;c;d;e] ->
        if e = 'X' && d = 'X' && c = 'X' && b = 'X' then 7
        else if e = 'X' && d = 'X' && c = 'X' then
          if a = b then 7 else 6
        else if e = 'X' && d = 'X' then
          if a = b && b = c then 7
          else if a = b || b = c then 6
          else 4
        else if e = 'X' then
           if a = b && b = c && c = d then 7
           else if a = b && b = c || b = c && c = d
           then 6
           else if a = b && c = d then 5
           else if a = b || b = c || c = d then 4
           else 2
        else
        if a = b && b = c && c = d && d = e then 7
        else if (a = b && b = c && c = d) || (b = c && c = d && d = e) then 6
        else if (a = b && b = c && d = e) || (a = b && c = d && d = e) then 5
        else if (a = b && b = c) || (b = c && c = d) || (c = d && d = e) then 4
        else if (a = b && c = d) || (a = b && d = e) || (b = c && d = e) then 3
        else if (a = b || b = c || c = d || d = e) then 2
        else 1
      | _ -> failwith "Incorrect hand!"
    in
    {
      point_tuple = (hand_point, char_to_score card1, char_to_score card2, char_to_score card3, char_to_score card4, char_to_score card5);
      bid = bid
    }

end

let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let () =
  file
  |> read_lines
  |> List.filter (fun x -> (String.length x > 1))
  |> List.map CamelCard.create_hand
  |> List.sort (fun x y -> compare CamelCard.(x.point_tuple) CamelCard.(y.point_tuple))
  |> List.mapi (fun x y -> (x+1) * CamelCard.(y.bid))
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
  file
  |> read_lines
  |> List.map (fun x -> Str.global_replace (Str.regexp "J") "X" x)
  |> List.filter (fun x -> (String.length x > 1))
  |> List.map CamelCard.create_hand
  |> List.sort (fun x y -> compare CamelCard.(x.point_tuple) CamelCard.(y.point_tuple))
  |> List.mapi (fun x y -> (x+1) * CamelCard.(y.bid))
  |> List.fold_left (fun x y -> x + y) 0
  |> string_of_int
  |> print_endline;
