open Lwt.Infix

let has_pork = List.mem "CONTIENT DU PORC"
let is_cold = List.mem "SE MANGE FROID"
let is_veggie = List.mem "VÉGÉTARIEN"
let is_vegan = List.mem "VEGAN"

let print_prod {Foodcheri.product_type; picture; chef; title; tags; price; _} =
  let pt = String.sub product_type 0 2 in
  let open ANSITerminal in
  let fg =
    if has_pork tags then
      magenta
    else if is_veggie tags then
      green
    else if is_vegan tags then
      blue
    else
      default
  in
  let st =
    fg ::
    if is_cold tags then
      []
    else
      [Bold]
  in
  (* FIXME test whether stdout is a terminal *)
  printf st "%s | %s\n" pt title;
  Lwt.return_unit

(* TODO cache *)
let () = Lwt_main.run (
  (*
  let%lwt shift = get_shift () in
  let%lwt html = get_menu_html shift in
  FIXME stream using from_signals?
  *)
  Foodcheri.get_menu {Foodcheri.idShift=42; deliveryDate="x"}
  >>= Lwt_list.iter_s print_prod
)
