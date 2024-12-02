(* @see: https://adventofcode.com/2024/day/2 *)

let parse input =
  let lines =
    input
    |> Core.String.split_on_chars ~on:[ '\n' ]
    |> List.filter (fun s -> not (Core.String.is_empty s))
  in
  let rec loop reports = function
    | [] -> reports
    | h :: t ->
        let nums =
          Core.String.split_on_chars ~on:[ ' ' ] h
          |> List.filter (fun s -> not (Core.String.is_empty s))
        in
        let rec inner_loop report = function
          | [] -> report
          | h' :: t' -> inner_loop (report @ [ int_of_string h' ]) t'
        in
        let int_reports = inner_loop [] nums in
        loop (reports @ [ int_reports ]) t
  in
  loop [] lines

let solve_part_one input =
  let list = parse input in
  let rec loop acc = function
    | [] -> acc
    | h :: t -> (
        match h with
        | f :: s :: rest ->
            let increasing = f < s in
            let rec check_report prev = function
              | [] -> true
              | curr :: rest ->
                  let diff = curr - prev in
                  if increasing then
                    diff > 0 && diff <= 3 && check_report curr rest
                  else diff < 0 && diff >= -3 && check_report curr rest
            in
            let safe = check_report f (s :: rest) in
            loop (if safe then acc + 1 else acc) t
        | [] | _ -> failwith "Invalid Report")
  in
  loop 0 list

let solve_part_two input =
  let list = parse input in
  let rec loop acc = function
    | [] -> acc
    | h :: t -> (
        match h with
        | f :: s :: rest ->
            let rec check_report prev increasing = function
              | [] -> true
              | curr :: rest ->
                  let diff = curr - prev in
                  if increasing && diff > 0 && diff <= 3 then
                    check_report curr true rest
                  else if (not increasing) && diff < 0 && diff >= -3 then
                    check_report curr false rest
                  else false
            in
            (* Bruh I had to brute force ;_; *)
            let rec try_each_removal idx =
              if idx >= List.length h then false
              else
                let removed = List.filteri (fun i _ -> i <> idx) h in
                match removed with
                | a :: b :: rest' ->
                    check_report a (a < b) (b :: rest')
                    || try_each_removal (idx + 1)
                | _ -> try_each_removal (idx + 1)
            in
            let safe =
              check_report f (f < s) (s :: rest) || try_each_removal 0
            in
            loop (if safe then acc + 1 else acc) t
        | [] | _ -> failwith "Invalid Report")
  in
  loop 0 list

  let solve input = 
    let solution1 = solve_part_one input
    and solution2 = solve_part_two input in
    Printf.sprintf "Solution 1: %d\nSolution 2: %d\n" solution1 solution2