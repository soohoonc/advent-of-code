(* @see: https://adventofcode.com/2024/day/14 *)

let parse input =
  let extract_p_v s =
    Scanf.sscanf s "p=%d,%d v=%d,%d" (fun p1 p2 v1 v2 -> ((p1, p2), (v1, v2)))
  in
  input |> Core.String.split_lines
  |> List.filter (fun s -> String.empty <> s)
  |> List.map extract_p_v

let solve_part_one input =
  let h = 103 and w = 101 in
  let quadrant_in sec robot =
    let p = fst robot and v = snd robot in
    let p_x = fst p and p_y = snd p in
    let end_x = (((p_x + (sec * fst v)) mod w) + w) mod w in
    let end_y = (((p_y + (sec * snd v)) mod h) + h) mod h in
    match (end_x, end_y) with
    | x, y when x < w / 2 && y < h / 2 -> 1
    | x, y when x < w / 2 && y > h / 2 -> 2
    | x, y when x > w / 2 && y < h / 2 -> 3
    | x, y when x > w / 2 && y > h / 2 -> 4
    | _ -> 0
  in
  let q1, q2, q3, q4 =
    input |> parse
    |> List.map (quadrant_in 100)
    |> List.fold_left
         (fun (q1, q2, q3, q4) id ->
           match id with
           | 1 -> (q1 + 1, q2, q3, q4)
           | 2 -> (q1, q2 + 1, q3, q4)
           | 3 -> (q1, q2, q3 + 1, q4)
           | 4 -> (q1, q2, q3, q4 + 1)
           | _ -> (q1, q2, q3, q4))
         (0, 0, 0, 0)
  in
  q1 * q2 * q3 * q4

(* Brute forced the pattern from some stuff i noticed while printing out *)
(* if i mod 101 = 48 && i mod 103 = 23 CRT this shit even *)

module PairMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let solve_part_two input =
  let h = 103 and w = 101 in
  let ff sec robot =
    let p = fst robot and v = snd robot in
    let p_x = fst p and p_y = snd p in
    let end_x = (((p_x + (sec * fst v)) mod w) + w) mod w
    and end_y = (((p_y + (sec * snd v)) mod h) + h) mod h in
    (end_x, end_y)
  in
  let robots = input |> parse in
  let rec count_non_symmetric seen acc = function
    | [] -> acc
    | (x, y) :: t -> (
        let x' = w - 1 - x in
        match PairMap.find_opt (x, y) seen with
        | Some _ -> count_non_symmetric seen acc t
        | None ->
            let new_seen = PairMap.add (x, y) true seen in
            let new_seen = PairMap.add (x', y) true new_seen in
            if x' = x then count_non_symmetric new_seen acc t
            else if PairMap.mem (x', y) seen then
              count_non_symmetric new_seen (acc - 1) t
            else count_non_symmetric new_seen (acc + 1) t)
  in
  let rec loop robots i acc =
    if i = h * w then acc
    else
      loop robots (i + 1)
        ((i, robots |> List.map (ff i) |> count_non_symmetric PairMap.empty 0)
        :: acc)
  in

  loop robots 0 []
  |> List.fold_left
       (fun (min_i, min_count) (i, count) ->
         if count < min_count then (i, count) else (min_i, min_count))
       (0, max_int)
  |> fst

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
