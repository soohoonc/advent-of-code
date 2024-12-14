(* @see: https://adventofcode.com/2024/day/11 *)

module StringMap = Map.Make (Core.String)

let parse input =
  input
  |> Core.String.split_on_chars ~on:[ '\n'; ' ' ]
  |> List.filter (fun s -> not (Core.String.is_empty s))

let solve_part_one input =
  let stones = parse input in
  let apply_rules stone =
    if int_of_string stone = 0 then "1"
    else if Core.String.length stone mod 2 = 0 then
      let f =
        int_of_string
          (Core.String.sub stone ~pos:0 ~len:(Core.String.length stone / 2))
      in
      let s =
        int_of_string
          (Core.String.sub stone
             ~pos:(Core.String.length stone / 2)
             ~len:(Core.String.length stone / 2))
      in
      string_of_int f ^ " " ^ string_of_int s
    else string_of_int (int_of_string stone * 2024)
  in
  let rec loop stones = function
    | 0 -> stones
    | n ->
        let new_stones =
          stones |> List.map apply_rules |> String.concat " "
          |> Core.String.split_on_chars ~on:[ ' ' ]
        in
        loop new_stones (n - 1)
  in
  let final_stones = loop stones 25 in
  List.length final_stones

let solve_part_two input =
  let stones = parse input in
  let cache = ref StringMap.empty in
  let rec blink_stone stone n =
    let num = int_of_string stone in
    let stone = string_of_int num in
    (* leading zeros *)
    let key = Printf.sprintf "%d:%d" num n in
    match StringMap.(find_opt key !cache) with
    | Some x -> x
    | None ->
        if n = 0 then 1
        else if num = 0 then blink_stone "1" (n - 1)
        else if Core.String.length stone mod 2 = 0 |> not then
          blink_stone (string_of_int (num * 2024)) (n - 1)
        else
          let half_len = Core.String.length stone / 2 in
          let f = Core.String.sub stone ~pos:0 ~len:half_len in
          let s = Core.String.sub stone ~pos:half_len ~len:half_len in
          let result = blink_stone f (n - 1) + blink_stone s (n - 1) in
          (cache := StringMap.(add key result !cache));
          result
  in
  let final_stones = stones |> List.map (fun stone -> blink_stone stone 75) in
  List.fold_left ( + ) 0 final_stones

(* let solve input = *)
let solve input =
  (* let input = "125 17" in *)
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
