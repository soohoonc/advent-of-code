open Core
open Async

let run ~year ~day =
  Utils.Input.get_input ~year ~day >>| function
  | Ok content -> (
      match Utils.File.get_solution ~year ~day with
      | Ok solve -> solve content
      | Error e -> Printf.sprintf "Error: %s\n" e)
  | Error e -> Printf.sprintf "Error Fetching Input: %s" e

let command =
  let year_param =
    Command.Param.anon (Command.Param.( %: ) "year" Command.Param.int)
  in
  let day_param =
    Command.Param.anon (Command.Param.( %: ) "day" Command.Param.int)
  in
  Command.async ~summary:"Start Advent of Code Solution"
    (Command.Param.map2 year_param day_param ~f:(fun year day () ->
         run ~year ~day >>= fun result ->
         print_endline result;
         return ()))

let () = Command_unix.run command