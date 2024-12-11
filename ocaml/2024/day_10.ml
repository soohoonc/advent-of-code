(* @see: https://adventofcode.com/2024/day/10 *)

module PairMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let parse input =
  input
  |> Core.String.split_on_chars ~on:[ '\n' ]
  |> List.filter (fun s -> not (Core.String.is_empty s))

let solve_part_one input =
  let map = parse input in
  let m = List.length map and n = Core.String.length (List.nth map 0) in
  let rec search x y acc curr seen =
    if x < 0 || y < 0 || x >= m || y >= n then (acc, seen)
    else
      let level =
        int_of_string (Core.String.make 1 (Core.String.get (List.nth map x) y))
      in
      let found =
        PairMap.(find_opt (x, y) seen) |> Option.value ~default:false
      in
      if level <> curr || found then (acc, seen)
      else
        let new_seen = PairMap.(add (x, y) true seen) in
        if level = 9 then (acc + 1, new_seen)
        else
          let u, new_seen = search (x + 1) y 0 (curr + 1) new_seen in
          let r, new_seen = search x (y + 1) 0 (curr + 1) new_seen in
          let d, new_seen = search (x - 1) y 0 (curr + 1) new_seen in
          let l, new_seen = search x (y - 1) 0 (curr + 1) new_seen in
          (u + d + l + r + acc, new_seen)
  in
  let rec loop x y acc =
    if x >= m && y >= n then acc
    else if y >= n then loop (x + 1) 0 acc
    else loop x (y + 1) (acc + fst (search x y 0 0 PairMap.empty))
  in
  loop 0 0 0

let solve_part_two input =
  let map = parse input in
  let m = List.length map and n = Core.String.length (List.nth map 0) in
  let rec search x y acc curr seen =
    if x < 0 || y < 0 || x >= m || y >= n then acc
    else
      let level =
        int_of_string (Core.String.make 1 (Core.String.get (List.nth map x) y))
      in
      let found =
        PairMap.(find_opt (x, y) seen) |> Option.value ~default:false
      in
      if level <> curr || found then acc
      else if level = 9 then acc + 1
      else
        let new_seen = PairMap.(add (x, y) true seen) in
        let u = search (x + 1) y 0 (curr + 1) new_seen in
        let r = search x (y + 1) 0 (curr + 1) new_seen in
        let d = search (x - 1) y 0 (curr + 1) new_seen in
        let l = search x (y - 1) 0 (curr + 1) new_seen in
        u + d + l + r + acc
  in
  let rec loop x y acc =
    if x >= m && y >= n then acc
    else if y >= n then loop (x + 1) 0 acc
    else loop x (y + 1) (acc + search x y 0 0 PairMap.empty)
  in
  loop 0 0 0

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
