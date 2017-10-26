open Lwt.Infix

(* TODO cache *)
let () = Lwt_main.run (
  (*
  let%lwt shift = get_shift () in
  let%lwt html = get_menu_html shift in
  FIXME stream using from_signals?
  *)
  let fmt {Foodcheri.product_type; picture; chef; title; tags; price; _} =
    let pt = String.sub product_type 0 2 in
    (*
    Printf.sprintf "%s | %5.2f€ | %s\n" pt price title
    *)
    Printf.sprintf "%s | %s\n" pt title
  in
  Foodcheri.get_menu {Foodcheri.idShift=42; deliveryDate="x"}
  >|= List.map fmt
  >>= Lwt_list.iter_s Lwt_io.print
)
