let file = "/tmp/input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let string_to_array a s =
  let l = String.length s in
  for i = 0 to l-1 do
    a.(i) <- String.get s i
  done

let transpose a rows cols =
  let b = Array.make_matrix cols rows ' ' in
  for i = 0 to cols - 1 do
    for j = 0 to rows - 1 do
      b.(i).(j) <- a.(j).(i)
    done
  done;
  b

let check_array_blank a =
  Array.fold_left (fun x y -> x && (y = '.')) true a

let find_galaxy_distances g1 g2 blank_rows blank_cols =
  match (g1, g2) with
  | ((x1, y1), (x2, y2)) ->
    let extra = (blank_rows |> List.find_all (fun x -> x > (min x1 x2) && x < (max x1 x2)) |> List.length) + (blank_cols |> List.find_all (fun y -> y > (min y1 y2) && y < (max y1 y2)) |> List.length) in
    let base_distance = (abs (x1 - x2)) + (abs (y1 - y2)) in
    (base_distance, extra)

let () =
  let n_rows = 140 in
  let n_cols = 140 in
  let a = Array.make_matrix n_rows n_cols ' ' in
  let lines = read_lines file in
  lines
  |> List.filter (fun x -> String.length x > 0)
  |> List.iteri (fun i x -> string_to_array a.(i) x);
  let blank_rows =
    Array.mapi (fun rowno row -> (rowno, check_array_blank row)) a
    |> Array.to_list
    |> List.filter (fun (_, x) -> x)
    |> List.map (fun (y, _) -> y)
    |> List.rev
  in
  let blank_cols =
    Array.mapi (fun rowno row -> (rowno, check_array_blank row)) (transpose a n_rows n_cols)
    |> Array.to_list
    |> List.filter (fun (_, x) -> x)
    |> List.map (fun (y, _) -> y)
    |> List.rev
  in
  let galaxy_locations = ref [] in
  for i = 0 to n_rows - 1 do
    for j = 0 to n_cols - 1 do
      galaxy_locations := if a.(i).(j) = '#' then (i, j)::!galaxy_locations else !galaxy_locations;
    done
  done;
  let sum_distances factor =
    List.map (fun x -> (x, !galaxy_locations)) !galaxy_locations
    |> List.map (fun (x, y) -> List.map (fun a -> (x, a)) y)
    |> List.concat
    |> List.map (fun ((x1, y1), (x2, y2)) ->
        if x1 < x2 then ((x1, y1), (x2, y2))
        else if x1 > x2 then ((x2, y2), (x1, y1))
        else if x1 = x2 && y1 < y2 then ((x1, y1), (x2, y2))
        else if x1 = x2 && y1 > y2 then ((x2, y2), (x1, y1))
        else ((x1, y1), (x2, y2)))
    |> List.filter (fun ((x1, y1), (x2, y2)) -> not (x1 = x2 && y1 = y2))
    |> List.sort_uniq (fun x y -> compare x y)
    |> List.map (fun (x, y) -> find_galaxy_distances x y blank_rows blank_cols)
    |> List.map (fun (x, y) -> x + y * (factor - 1))
    |> List.fold_left (fun x y -> x + y) 0
    |> string_of_int
    |> print_endline
  in
  sum_distances 2;
  sum_distances 1000000;
