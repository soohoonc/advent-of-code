(* @see: https://adventofcode.com/2025/day/5 *)
let parse input =
  match
    String.split_on_char '\n' input
    |> List.partition (fun s -> String.contains s '-')
  with
  | ranges, ids ->
      let ranges =
        ranges |> List.map (fun s -> Scanf.sscanf s "%d-%d" (fun a b -> (a, b)))
      in
      let ids =
        ids |> List.filter_map (fun s -> int_of_string_opt (String.trim s))
      in
      (ranges, ids)

let solve_part_one input =
  let ranges, ids = parse input in
  let m = List.length ranges and n = List.length ids in
  let rec id_loop acc = function
    | i when i = n -> acc
    | i ->
        let id = List.nth ids i in
        let rec ranges_loop = function
          | j when j = m -> 0
          | j ->
              let range = List.nth ranges j in
              if id >= fst range && id <= snd range then 1
              else ranges_loop (j + 1)
        in
        id_loop (acc + ranges_loop 0) (i + 1)
  in
  id_loop 0 0

let solve_part_two input =
  let ranges = parse input |> fst in
  let merge_ranges ranges =
    let sorted = List.sort compare ranges in
    List.fold_left
      (fun acc (l, h) ->
        match acc with
        | [] -> [ (l, h) ]
        | (lo, hi) :: rest ->
            if l <= hi then (lo, max h hi) :: rest else (l, h) :: acc)
      [] sorted
    |> List.rev
  in
  let merged_ranges = merge_ranges ranges in
  List.fold_left
    (fun acc range -> acc + snd range - fst range + 1)
    0 merged_ranges

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
