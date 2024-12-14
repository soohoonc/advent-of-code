(* @see: https://adventofcode.com/2024/day/13 *)

let parse input =
  let lines =
    Core.String.split_lines input |> List.filter (fun s -> String.empty <> s)
  in
  let extract_coords s =
    if Core.String.is_prefix s ~prefix:"Prize" then
      Scanf.sscanf s "Prize: X=%d, Y=%d" (fun x y -> (x, y))
    else if Core.String.is_prefix s ~prefix:"Button" then
      Scanf.sscanf s "Button %c: X%d, Y%d" (fun _ x y -> (x, y))
    else failwith ("Invalid input: " ^ s)
  in
  let rec loop acc = function
    | [] -> acc
    | h :: t -> loop (extract_coords h :: acc) t
  in
  loop [] lines |> List.rev

let solve_part_one input =
  let vals = parse input in
  let determinant v1 v2 = (fst v1 * snd v2) - (snd v1 * fst v2) in
  let get_solution v1 v2 v =
    let det = determinant v1 v2 in
    if det = 0 then 0
    else
      let discriminant_x = (snd v2 * fst v) - (fst v2 * snd v) in
      let discriminant_y = (fst v1 * snd v) - (snd v1 * fst v) in
      let x = discriminant_x / det in
      let y = discriminant_y / det in
      if
        x >= 0 && y >= 0 && x <= 100 && y <= 100
        && (x * fst v1) + (y * fst v2) = fst v
        && (x * snd v1) + (y * snd v2) = snd v
      then (3 * x) + y
      else 0
  in
  let rec loop i acc =
    if i >= List.length vals then acc
    else
      let v1 = List.nth vals i
      and v2 = List.nth vals (i + 1)
      and v = List.nth vals (i + 2) in
      let solution = get_solution v1 v2 v in
      loop (i + 3) (acc + solution)
  in
  loop 0 0

let solve_part_two input =
  let vals = parse input in
  let determinant v1 v2 = (fst v1 * snd v2) - (snd v1 * fst v2) in
  let get_solution v1 v2 v =
    let det = determinant v1 v2 in
    if det = 0 then 0
    else
      let v1_t = fst v + 10000000000000 and v2_t = snd v + 10000000000000 in
      let discriminant_x = (snd v2 * v1_t) - (fst v2 * v2_t) in
      let discriminant_y = (fst v1 * v2_t) - (snd v1 * v1_t) in
      let x = discriminant_x / det in
      let y = discriminant_y / det in
      if
        x >= 0 && y >= 0
        && (x * fst v1) + (y * fst v2) = v1_t
        && (x * snd v1) + (y * snd v2) = v2_t
      then (3 * x) + y
      else 0
  in
  let rec loop i acc =
    if i >= List.length vals then acc
    else
      let v1 = List.nth vals i
      and v2 = List.nth vals (i + 1)
      and v = List.nth vals (i + 2) in
      let solution = get_solution v1 v2 v in
      loop (i + 3) (acc + solution)
  in
  loop 0 0

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
(* @see: https://adventofcode.com/2024/day/1 *)
