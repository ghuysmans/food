open Lwt.Infix

let default_to default = function
  | Some x -> x
  | None -> default

let tee s =
  Lwt_io.print s >|= fun () ->
  s
