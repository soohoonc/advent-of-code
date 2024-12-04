open Y2024

module type Day_solution = sig
  val solve : string -> string
end

let get_solution ~year ~day =
  try
    let solution_function =
      match year with
      | 2024 -> (
          match day with
          | 1 -> Day_01.solve
          | 2 -> Day_02.solve
          | 3 -> Day_03.solve
          | 4 -> Day_04.solve
          (* | 5 -> Day_05.solve *)
          | _ when day <= 25 ->
              failwith
                (Printf.sprintf "Day %d not implemented for year %d" day year)
          | _ -> failwith "Day must be between 1 and 25")
      | _ -> failwith (Printf.sprintf "Year %d not implemented" year)
    in
    Ok solution_function
  with _ ->
    Error (Printf.sprintf "Solution not found for year %d day %d" year day)