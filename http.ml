open Lwt.Infix
exception HttpError of int

let call meth path query =
  let headers = Cohttp.Header.init_with "X-Requested-With" "XMLHttpRequest" in
  let uri query =
    Uri.make ~scheme:"https" ~host:"www.foodcheri.com" ~path ~query ()
  in
  (match meth with
  | `GET ->
    Cohttp_lwt_unix.Client.get ~headers (uri query)
  | `POST ->
    let body = Uri.encoded_of_query query |> Cohttp_lwt_body.of_string in
    let headers = Cohttp.Header.add headers
      "Content-type"
      "application/x-www-form-urlencoded"
    in
    Cohttp_lwt_unix.Client.post ~body ~headers (uri [])
  ) >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK -> body |> Cohttp_lwt_body.to_string
  | status -> raise @@ HttpError (Cohttp.Code.code_of_status status)
