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
  (* let h = 7 and w = 11 in *)
  let quadrant_in sec robot =
    let p = fst robot and v = snd robot in
    let p_x = fst p and p_y = snd p in
    let end_x = (((p_x + (sec * fst v)) mod w) + w) mod w in
    let end_y = (((p_y + (sec * snd v)) mod h) + h) mod h in
    (* Printf.printf "Robot starting at (%d,%d) ends at (%d,%d)\n" p_x p_y end_x
       end_y; *)
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
  (* Printf.printf
     "Quadrant 1: %d\nQuadrant 2: %d\nQuadrant 3: %d\nQuadrant 4: %d\n" q1 q2 q3
     q4; *)
  q1 * q2 * q3 * q4

let solve_part_two input =
  let h = 103 and w = 101 in
  (* let h = 7 and w = 11 in *)
  let quadrant_in sec robot =
    let p = fst robot and v = snd robot in
    let p_x = fst p and p_y = snd p in
    let end_x = (((p_x + (sec * fst v)) mod w) + w) mod w in
    let end_y = (((p_y + (sec * snd v)) mod h) + h) mod h in
    (* Printf.printf "Robot starting at (%d,%d) ends at (%d,%d)\n" p_x p_y end_x
       end_y; *)
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
  (* Printf.printf
     "Quadrant 1: %d\nQuadrant 2: %d\nQuadrant 3: %d\nQuadrant 4: %d\n" q1 q2 q3
     q4; *)
  q1 * q2 * q3 * q4

let solve input =
  (* let solve _ =
     let input =
       "p=0,4 v=3,-3\n\
        p=6,3 v=-1,-3\n\
        p=10,3 v=-1,2\n\
        p=2,0 v=2,-1\n\
        p=0,0 v=1,3\n\
        p=3,0 v=-2,-2\n\
        p=7,6 v=-1,-3\n\
        p=3,0 v=-1,-2\n\
        p=9,3 v=2,3\n\
        p=7,3 v=-1,2\n\
        p=2,4 v=2,-3\n\
        p=9,5 v=-3,-3"
     in *)
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
