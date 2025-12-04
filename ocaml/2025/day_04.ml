(* @see: https://adventofcode.com/2025/day/4 *)
let parse input =
  input |> Core.String.split_lines
  |> List.map (fun s -> Bytes.of_string s)
  |> Array.of_list

let directions =
  [| (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (-1, 1); (1, -1); (1, 1) |]

let count_adj map m n x y =
  let count = ref 0 in
  for d = 0 to 7 do
    let dx, dy = directions.(d) in
    let x', y' = (x + dx, y + dy) in
    if x' >= 0 && y' >= 0 && x' < m && y' < n && Bytes.get map.(x') y' = '@'
    then incr count
  done;
  !count

let solve_part_one input =
  let map = parse input in
  let m = Array.length map and n = Bytes.length map.(0) in
  let count = ref 0 in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      if Bytes.get map.(i) j = '@' && count_adj map m n i j < 4 then incr count
    done
  done;
  !count

let solve_part_two input =
  let map = parse input in
  let m = Array.length map and n = Bytes.length map.(0) in
  let total = ref 0 in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        if Bytes.get map.(i) j = '@' && count_adj map m n i j < 4 then (
          Bytes.set map.(i) j '.';
          incr total;
          changed := true)
      done
    done
  done;
  !total

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
