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

(* TODO implement group_by *)
let products l =
  let open Tyxml in
  let fmt {Foodcheri.title; tags; price; _} =
    Html.p [Html.pcdata title]
  in
  let f (l, prev) ({Foodcheri.product_type; _} as prod) =
    match prev with
    | None -> [Html.h2 [Html.pcdata product_type]], Some prod
    | Some {Foodcheri.product_type = pt'; _} ->
      if product_type = pt' then
        (fmt prod) :: l,
        Some prod
      else
        fmt prod :: Html.h2 [Html.pcdata product_type] :: l,
        Some prod
  in
  List.fold_left f ([], None) l |>
  fst |>
  List.rev


let besport {Foodcheri.product_type; price} =
  match product_type with
  | "STARTER" ->
    price <= 4.0
  | "MAIN_COURSE" ->
    price <= 12.0
  | "DESSERT" ->
    price <= 4.0
  | _ ->
    false

let () = Lwt_main.run (
  let%lwt shift = Foodcheri.get_shift () in
  let%lwt raw = Foodcheri.get_menu shift in
  let menu = List.filter besport raw in
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "-html" then (
    let fmt = Format.formatter_of_out_channel stdout in
    let title = "FoodChéri " ^ shift.Foodcheri.deliveryDate in
    let page = Template.page title (products menu) in
    Tyxml.Html.pp () fmt page;
    Format.pp_print_flush fmt ();
    Lwt.return_unit
  )
  else
    Lwt_list.iter_s print_prod menu
)
