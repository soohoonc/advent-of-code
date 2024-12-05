(* @see: https://adventofcode.com/2024/day/4 *)

module IntMap = Map.Make (Int)

let parse input =
  let lines = Core.String.split_on_chars ~on:[ '\n' ] input in
  let rec parse_rules acc idx =
    if idx >= List.length lines || String.trim (List.nth lines idx) = "" then
      (acc, idx + 1)
    else
      let line = List.nth lines idx in
      match Core.String.split_on_chars ~on:[ '|' ] line with
      | [ num1; num2 ] ->
          let n1 = int_of_string num1 and n2 = int_of_string num2 in
          let existing =
            match IntMap.find_opt n1 acc with Some l -> l | None -> []
          in
          let new_acc = IntMap.(add n1 (n2 :: existing) acc) in
          parse_rules new_acc (idx + 1)
      | _ -> parse_rules acc (idx + 1)
  in
  let rec parse_pages idx acc =
    if idx >= List.length lines then acc
    else
      let line = List.nth lines idx in
      if String.trim line = "" then acc
      else
        let page_list =
          Core.String.split_on_chars ~on:[ ',' ] line |> List.map int_of_string
        in
        parse_pages (idx + 1) (page_list :: acc)
  in
  let rules, index = parse_rules IntMap.empty 0 in
  let pages = parse_pages index [] |> List.rev in
  (rules, pages)

let solve_part_one input =
  let rules, pages = parse input in
  let mid page_list = List.nth page_list (List.length page_list / 2) in
  let rec pages_loop acc idx =
    if idx >= List.length pages then acc
    else
      let current_pages = List.nth pages idx in
      let rec page_loop i seen =
        if i >= List.length current_pages then true
        else
          let page = List.nth current_pages i in
          let page_rules = IntMap.(find_opt page rules) in
          match page_rules with
          | None ->
              let new_seen = IntMap.(add page 0 seen) in
              page_loop (i + 1) new_seen
          | Some page_rules ->
              let rec rule_search = function
                | [] -> true
                | rule :: t -> (
                    match IntMap.find_opt rule seen with
                    | Some _ -> false
                    | None -> rule_search t)
              in
              let passes_rules = rule_search page_rules in
              let new_seen = IntMap.(add page 0 seen) in
              if passes_rules then page_loop (i + 1) new_seen else false
      in
      let valid = page_loop 0 IntMap.empty in
      let mid_val = if valid then mid current_pages else 0 in
      let new_acc = mid_val + acc in
      pages_loop new_acc (idx + 1)
  in
  pages_loop 0 0

let solve_part_two input =
  let rules, pages = parse input in
  let mid page_list = List.nth page_list (List.length page_list / 2) in
  let sort unsorted_pages =
    let compare_with_rules a b =
      match IntMap.find_opt a rules with
      | Some rule_list when List.mem b rule_list -> -1
      | _ -> (
          match IntMap.find_opt b rules with
          | Some rule_list when List.mem a rule_list -> 1
          | _ -> 0)
    in
    List.sort compare_with_rules unsorted_pages
  in
  let rec pages_loop acc idx =
    if idx >= List.length pages then acc
    else
      let current_pages = List.nth pages idx in
      let rec page_loop i seen =
        if i >= List.length current_pages then true
        else
          let page = List.nth current_pages i in
          let page_rules = IntMap.(find_opt page rules) in
          match page_rules with
          | None ->
              let new_seen = IntMap.(add page 0 seen) in
              page_loop (i + 1) new_seen
          | Some page_rules ->
              let rec rule_search = function
                | [] -> true
                | rule :: t -> (
                    match IntMap.find_opt rule seen with
                    | Some _ -> false
                    | None -> rule_search t)
              in
              let passes_rules = rule_search page_rules in
              let new_seen = IntMap.(add page 0 seen) in
              if passes_rules then page_loop (i + 1) new_seen else false
      in
      let valid = page_loop 0 IntMap.empty in
      let mid_val = if not valid then mid (sort current_pages) else 0 in
      let new_acc = mid_val + acc in
      pages_loop new_acc (idx + 1)
  in
  pages_loop 0 0

let solve input =
  let solution1 = solve_part_one input and solution2 = solve_part_two input in
  Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2
