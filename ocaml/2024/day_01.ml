(* @see: https://adventofcode.com/2024/day/1 *)

let parse input =
  let lines =
    input
    |> Core.String.split_on_chars ~on:[ '\n' ]
    |> List.filter (fun s -> not (Core.String.is_empty s))
  in
  let rec loop l1 l2 = function
    | [] -> (l1, l2)
    | h :: t -> (
        let nums =
          Core.String.split_on_chars ~on:[ ' ' ] h
          |> List.filter (fun s -> not (Core.String.is_empty s))
        in
        match nums with
        | [ n1; n2 ] ->
            let num1 = int_of_string n1 and num2 = int_of_string n2 in
            loop (num1 :: l1) (num2 :: l2) t
        | _ -> loop l1 l2 t)
  in
  loop [] [] lines

let solve_part_one input =
  let list1, list2 = parse input in
  let rec get_diff acc lists =
    match lists with
    | h1 :: t1, h2 :: t2 -> get_diff (acc + abs (h1 - h2)) (t1, t2)
    | _ -> acc
  in
  get_diff 0 (List.sort Int.compare list1, List.sort Int.compare list2)

module IntMap = Map.Make (Int)

let solve_part_two input =
  let list1, list2 = parse input in
  let counter =
    List.fold_left
      (fun acc num ->
        IntMap.update num
          (function None -> Some 1 | Some count -> Some (count + 1))
          acc)
      IntMap.empty list2
  in
  List.fold_left
    (fun acc num ->
      match IntMap.find_opt num counter with
      | None -> acc
      | Some count -> acc + (num * count))
    0 list1

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
