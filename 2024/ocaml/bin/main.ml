open Core
open Async

let () =
  let session =
    Dotenv.export ();
    Sys.getenv "AOC_SESSION"
    |> Option.value_exn ~message:"AOC_SESSION not found in environment or .env file"
  in
  Command.async ~summary:"Start Advent of Code Solution"
    (Command.Param.return (fun () ->
         Aoc.Input.get_input ~session ~year:2024 ~day:1 >>| function
         | Ok content -> 
            let part_one = Aoc.Day_one.solve_part_one content in
            let part_two = Aoc.Day_one.solve_part_two content in
            Core.Printf.printf "Part 1: %d\nPart 2: %d\n" part_one part_two
         | Error e -> print_endline @@ Printf.sprintf "Error Fetching Input: %s" e))
  |> Command_unix.run
