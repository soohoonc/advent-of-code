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
          | 1 -> Ok Y2024.Day_01.solve
          | 2 -> Ok Y2024.Day_02.solve
          | 3 -> Ok Y2024.Day_03.solve
          | 4 -> Ok Y2024.Day_04.solve
          | 5 -> Ok Y2024.Day_05.solve
          | 6 -> Ok Y2024.Day_06.solve
          | 7 -> Ok Y2024.Day_07.solve
          | 8 -> Ok Y2024.Day_08.solve
          | 9 -> Ok Y2024.Day_09.solve
          | 10 -> Ok Y2024.Day_10.solve
          | 11 -> Ok Y2024.Day_11.solve
          | 12 -> Ok Y2024.Day_12.solve
          | 13 -> Ok Y2024.Day_13.solve
          | 14 -> Ok Y2024.Day_14.solve
          | 15 -> Ok Y2024.Day_15.solve
          | 16 -> Ok Y2024.Day_16.solve
          | 17 -> Ok Y2024.Day_17.solve
          | 18 -> Ok Y2024.Day_18.solve
          | 19 -> Ok Y2024.Day_19.solve
          | 20 -> Ok Y2024.Day_20.solve
          | 21 -> Ok Y2024.Day_21.solve
          | 22 -> Ok Y2024.Day_22.solve
          | 23 -> Ok Y2024.Day_23.solve
          | _ ->
              Error
                (Printf.sprintf "Solution not found for year %d day %d" year day)
          )
      | 2025 -> (
          match day with
          | 1 -> Ok Y2025.Day_01.solve
          | 2 -> Ok Y2025.Day_02.solve
          (* | 3 -> Ok Y2025.Day_03.solve
             | 4 -> Ok Y2025.Day_04.solve
             | 5 -> Ok Y2025.Day_05.solve
             | 6 -> Ok Y2025.Day_06.solve
             | 7 -> Ok Y2025.Day_07.solve
             | 8 -> Ok Y2025.Day_08.solve
             | 9 -> Ok Y2025.Day_09.solve
             | 10 -> Ok Y2025.Day_10.solve
             | 11 -> Ok Y2025.Day_11.solve
             | 12 -> Ok Y2025.Day_12.solve *)
          | _ ->
              Error
                (Printf.sprintf "Solution not found for year %d day %d" year day)
          )
      | _ -> Error (Printf.sprintf "No solutions for year: %d" year)
  with e ->
    Error (Printf.sprintf "Error loading solution: %s" (Printexc.to_string e))