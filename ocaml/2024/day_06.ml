(* @see: https://adventofcode.com/2024/day/6 *)

module PairMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

module TripletMap = Map.Make (struct
  type t = int * int * int

  let compare = compare
end)

let parse input = Core.String.split_on_chars ~on:[ '\n'; '\r' ] input

let inbounds x y lines =
  x >= 0 && y >= 0
  && x < List.length lines
  && y < String.length (List.nth lines x)

let rec find_guard x y input =
  if x >= List.length input then failwith "Guard Not Found"
  else if y >= Core.String.length (List.nth input x) then
    find_guard (x + 1) 0 input
  else
    match Core.String.get (List.nth input x) y with
    | '^' -> (x, y)
    | _ -> find_guard x (y + 1) input

let solve_part_one input =
  let input = parse input in
  let x, y = find_guard 0 0 input in
  let directions = [ (-1, 0); (0, 1); (1, 0); (0, -1) ] in
  let rec traverse x y dir_idx seen =
    let direction = List.nth directions dir_idx in
    let x_new = x + fst direction and y_new = y + snd direction in
    if not (inbounds x_new y_new input) then PairMap.cardinal seen + 1
    else
      match Core.String.get (List.nth input x_new) y_new with
      | '#' ->
          let new_dir_idx = (dir_idx + 1) mod 4 in
          traverse x y new_dir_idx seen
      | _ ->
          let new_seen = PairMap.add (x, y) true seen in
          traverse x_new y_new dir_idx new_seen
  in
  traverse x y 0 PairMap.empty

(* in other words count the number of paths at a 90 deg clockwise rotation to the current path*)
let solve_part_two input =
  let input = parse input in
  let start_x, start_y = find_guard 0 0 input in
  let directions = [ (-1, 0); (0, 1); (1, 0); (0, -1) ] in
  (* First get the main path *)
  let main_path =
    let rec traverse x y dir_idx seen =
      let direction = List.nth directions dir_idx in
      let x_new = x + fst direction and y_new = y + snd direction in
      if not (inbounds x_new y_new input) then seen
      else
        match Core.String.get (List.nth input x_new) y_new with
        | '#' ->
            let new_dir_idx = (dir_idx + 1) mod 4 in
            traverse x y new_dir_idx seen
        | _ ->
            let new_seen = PairMap.add (x, y) true seen in
            traverse x_new y_new dir_idx new_seen
    in
    traverse start_x start_y 0 PairMap.empty
  in
  (* fucking hell *)
  PairMap.fold
    (fun (x, y) _ acc ->
      let rec follow_path curr_x curr_y dir_idx seen =
        let direction = List.nth directions dir_idx in
        let x_new = curr_x + fst direction and y_new = curr_y + snd direction in
        if TripletMap.mem (curr_x, curr_y, dir_idx) seen then true
        else if not (inbounds x_new y_new input) then false
        else if
          (x_new, y_new) = (x, y)
          || Core.String.get (List.nth input x_new) y_new = '#'
        then
          let new_dir_idx = (dir_idx + 1) mod 4 in
          follow_path curr_x curr_y new_dir_idx
            (TripletMap.add (curr_x, curr_y, dir_idx) true seen)
        else
          follow_path x_new y_new dir_idx
            (TripletMap.add (curr_x, curr_y, dir_idx) true seen)
      in
      if follow_path start_x start_y 0 TripletMap.empty then acc + 1 else acc)
    main_path 0

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
