open Core
open Async

let get_input ~year ~day ~session =
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
  | 200 -> Ok content
  | _ -> Error (sprintf "HTTP status code %d, message: %s" status content)
