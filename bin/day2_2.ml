module Game = struct
  type draw = {
    blue : int ;
    red : int ;
    green : int ;
  }

  type t = {
    game_number : int ;
    draws : draw list ;
  }

  let parse_game s =
    let rec list_to_game l game =
      match l with
      | [] -> game
      | a::rest ->
        match a with
        | [s; "blue"] -> list_to_game rest { game with blue = int_of_string s }
        | [s; "red"] -> list_to_game rest { game with red = int_of_string s }
        | [s; "green"] -> list_to_game rest { game with green = int_of_string s }
        | _ -> game
    in
    let r = Str.regexp "^Game \\([0-9][0-9]*\\)" in
    let game_number = (if Str.string_match r s 0 then
                         Str.matched_group 1 s |> int_of_string else -1)
    in
    let r = Str.regexp "^[^:]*: \\(.+\\)" in
    if (Str.string_match r s 0)
    then
      let mydraws =
        (Str.matched_group 1 s)
         |> String.split_on_char ';'
         |> List.map (fun x -> String.split_on_char ',' x)
         |> List.map (fun x -> List.map String.trim x)
         |> List.map (fun x -> List.map (String.split_on_char ' ') x)
         |> List.map (fun x -> list_to_game x { red = 0; green = 0; blue = 0} ) in
      { game_number = game_number ; draws = mydraws }
    else { game_number = -1 ; draws = [] }

  let get_game_power game =
    let rec get_game_power_helper max_draw =
      function
      | [] -> max_draw.red * max_draw.blue * max_draw.green
      | a::rest ->
        get_game_power_helper
          {
            green = max a.green max_draw.green;
            blue = max a.blue max_draw.blue;
            red = max a.red max_draw.red;
          }
          rest
    in
    get_game_power_helper { red = 0; green = 0; blue = 0; } game.draws

  let get_max_balls game =
    let rec get_max_balls_helper l draw_counts =
      match l with
      | [] -> draw_counts
      | a::rest -> get_max_balls_helper rest
                     { red = max draw_counts.red a.red;
                     green = max draw_counts.green a.green;
                     blue = max draw_counts.blue a.blue }
    in
    get_max_balls_helper game.draws { red = 0; green = 0; blue = 0; }

end

let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let () =
  let lines = file |> read_lines |> List.filter (fun x -> (String.length x > 1))
  in
  lines
  |> List.map Game.parse_game
  |> List.map Game.(fun x -> (x.game_number, get_max_balls x))
  |> List.filter Game.(fun (_, y) -> y.red <= 12 && y.green <= 13 && y.blue <= 14)
  |> List.fold_left (fun y (x, _) -> x + y) 0
  |> string_of_int
  |> print_endline;
  lines
  |> List.map Game.parse_game
  |> List.fold_left Game.(fun x y -> x + (get_game_power y)) 0
  |> string_of_int
  |> print_endline
