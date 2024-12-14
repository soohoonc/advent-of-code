open Y2024

module type Day_solution = sig
  val solve : string -> string
end

let get_solution ~year ~day =
  try
    if day <= 0 || day > 25 then Error "Day must be between 1 and 25"
    else
      match year with
      | 2024 -> (
          match day with
          | 1 -> Ok Day_01.solve
          | 2 -> Ok Day_02.solve
          | 3 -> Ok Day_03.solve
          | 4 -> Ok Day_04.solve
          | 5 -> Ok Day_05.solve
          | 6 -> Ok Day_06.solve
          | 7 -> Ok Day_07.solve
          | 8 -> Ok Day_08.solve
          | 9 -> Ok Day_09.solve
          | 10 -> Ok Day_10.solve
          | 11 -> Ok Day_11.solve
          | 13 -> Ok Day_13.solve
          | _ ->
              Error
                (Printf.sprintf "Solution not found for year %d day %d" year day)
          )
      | _ -> Error (Printf.sprintf "No solutions for year: %d" year)
  with e ->
    Error (Printf.sprintf "Error loading solution: %s" (Printexc.to_string e))