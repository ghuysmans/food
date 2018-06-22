open Lwt.Infix

let default_to default = function
  | Some x -> x
  | None -> default

let tee s =
  Lwt_io.print s >|= fun () ->
  s

(* adapted from https://stackoverflow.com/a/22143976 *)
let count_dup l =
  match List.sort compare l with
  | [] -> []
  | h :: t ->
    let counted, current, count =
      List.fold_left (fun (counted, current, count) x ->
        if x = current then
          counted, current, count+1
        else
          (* we're done with the current element *)
          (current, count) :: counted, x, 1
      ) ([], h, 1) t
    in
    (current, count) :: counted

let checkbox name ?(value="1") checked =
  let open Tyxml.Html in
  let a = [a_input_type `Checkbox; a_name name; a_value value] in
  input ~a:(if checked then a_checked () :: a else a) ()
