(** HTTP library *)

(** HTTP error code, including redirects *)
exception HttpError of int


(** [call] performs an HTTP request.
    [~cache:true] will reuse cached non-GET responses (nice for testing).
    [~cache:false] will ignore cached responses. *)
val call:
  [`GET | `POST] ->
  string ->
  (string * string list) list ->
  string Lwt.t
