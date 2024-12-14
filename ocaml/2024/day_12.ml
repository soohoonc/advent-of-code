(* @see: https://adventofcode.com/2024/day/12 *)

let parse input = input |> Core.String.split_lines |> List.filter (fun s -> String.empty <> s)
module PairMap = Map.Make (struct
  type t = int * int
  let compare = compare
end)

let solve_part_one input =
  let map = parse input in
  let m = List.length map and n = Core.String.length (List.nth map 0) in
  let seen = ref PairMap.empty in
  let price x y = 
    let neighbor x y region = 
      let r = if x < m - 1 && region = (Core.String.get (List.nth map x+1) y) then 1 else 0 
      and l = if x > 0 && region = Core.String.get (List.nth map x-1) y) then 1 else 0 
      and d = if y < n - 1 && region = Core.String.get (List.nth map x) y+1) then 1 else 0 
      and u = if y > 0 && region = Core.String.get (List.nth map x) y-1) then 1 else 0 
      in r + l + d + u
    in
    let rec scan_region x y region acc = 
    match PairMap.(find_opt (x,y) !seen)
      | Some _ -> (0, 0) 
      | None -> 
          if (x >= m || y >= n || x < 0 || y < 0) then (0, 0) else
          let r = scan_region (x + 1) y acc 
          and l = scan_region (x - 1) y acc 
          and d = scan_region x (y + 1) acc 
          and u = scan_region x (y - 1) acc 
      in
      seen := PairMap.(add (x, y) region !seen);
      (fst r + fst l + fst d + fst u + acc + 1, snd r + snd l + snd d + snd u + acc + 4 - neighbor region x y)
    in let (area, perimeter) = scan_region x y (Core.String.get (List.nth map x) y) 0 in
    area * perimeter
  in
  let rec loop x y acc = 
    if x >= m then acc 
    else if y >= n then loop (x + 1) 0 acc
    else loop x (y + 1) (acc + price x y) 
  in
  loop 0 0 0
    


let solve_part_two _ = 0

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
