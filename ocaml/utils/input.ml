open Core
open Async

let get_input ~year ~day =
  let input_file_name = Core.Printf.sprintf "./%d/inputs/%02d.txt" year day in
  match Core.In_channel.read_all input_file_name with
  | content -> return (Ok content)
  | exception _ -> (
      let session =
        Dotenv.export ~path:"../.env" ();
        Sys.getenv "AOC_SESSION"
        |> Option.value_exn
             ~message:"AOC_SESSION not found in environment or .env file"
      in
      let url =
        Printf.sprintf "https://adventofcode.com/%d/day/%d/input" year day
      in
      let uri = Uri.of_string url in
      let headers =
        Cohttp.Header.init () |> fun h ->
        Cohttp.Header.add h "Cookie" (Printf.sprintf "session=%s" session)
      in
      Cohttp_async.Client.get ~headers uri >>= fun (res, body) ->
      let status = Cohttp.Response.status res |> Cohttp.Code.code_of_status in
      Cohttp_async.Body.to_string body >>| fun content ->
      match status with
      | 200 ->
          Core.Out_channel.write_all input_file_name ~data:content;
          Ok content
      | _ -> Error (sprintf "HTTP status code %d, message: %s" status content))
