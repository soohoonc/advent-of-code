(* @see: https://adventofcode.com/2024/day/5 *)

module IntMap = Map.Make (Int)

let parse input =
  let rules, pages =
    Core.String.split_on_chars ~on:[ '\n'; '\r' ] input
    |> List.filter (fun s -> String.trim s <> "")
    |> List.fold_left
         (fun (rules, pages) line ->
           match Core.String.split_on_chars ~on:[ '|' ] line with
           | [ n1; n2 ] ->
               let n1 = int_of_string n1 and n2 = int_of_string n2 in
               let rules' =
                 IntMap.update n1
                   (function None -> Some [ n2 ] | Some l -> Some (n2 :: l))
                   rules
               in
               (rules', pages)
           | _ ->
               let page =
                 Core.String.split_on_chars ~on:[ ',' ] line
                 |> List.map int_of_string
               in
               (rules, page :: pages))
         (IntMap.empty, [])
  in
  (rules, List.rev pages)

let mid page_list = List.nth page_list (List.length page_list / 2)

let sorted unsorted_pages rules =
  let compare_with_rules a b =
    match IntMap.find_opt a rules with
    | Some rule_list when List.mem b rule_list -> -1
    | _ -> (
        match IntMap.find_opt b rules with
        | Some rule_list when List.mem a rule_list -> 1
        | _ -> 0)
  in
  List.sort compare_with_rules unsorted_pages

let solve_part_one input =
  let rules, pages = parse input in
  let rec pages_loop acc = function
    | [] -> acc
    | page :: rest ->
        if sorted page rules = page then pages_loop (acc + mid page) rest
        else pages_loop acc rest
  in
  pages_loop 0 pages

let solve_part_two input =
  let rules, pages = parse input in
  let rec pages_loop acc = function
    | [] -> acc
    | page :: rest ->
        if sorted page rules <> page then
          pages_loop (acc + mid (sorted page rules)) rest
        else pages_loop acc rest
  in
  pages_loop 0 pages

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
