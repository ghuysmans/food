open Lwt.Infix

(* FIXME useless yojson *)
type shift = {
  idShift: int;
  deliveryDate: string;
} [@@deriving yojson]

(* FIXME useless yojson *)
type selected_shift = {
  success: bool;
  selectedShift: shift option;
} [@@deriving yojson]

type product = {
  product_type: string;
  is_main: bool;
  id: int;
  picture: string;
  chef: string option; (* FoodChéri -> None *)
  title: string;
  tags: string list;
  price: float;
} [@@deriving show]


let get_shift () =
  Http.call `GET "/ajaxGetSelectedShift" []
  >|= Yojson.Safe.from_string
  >|= selected_shift_of_yojson >>= function
    | Error e -> Lwt.fail_with e
    | Ok r ->
      if r.success then
        match r.selectedShift with
        | None -> assert false
        | Some s -> Lwt.return s
      else
        Lwt.fail_with "ajaxGetSelectedShift"

let get_menu_html ?delivery shift =
  let delivery = delivery |> Utils.default_to shift.deliveryDate in
  let params = [
    "idSelectedShift", [string_of_int shift.idShift];
    "deliveryDate", [delivery]
  ] in
  Http.call `POST "/ajaxGetProductPublication" params
  >|= Yojson.Safe.from_string
  >|= Yojson.Safe.Util.member "page"
  >|= Yojson.Safe.Util.to_string

let get_menu ?delivery shift =
  (*
  FIXME
  let%lwt html = get_menu_html ?delivery shift in
  *)
  let open Soup in
  let soup = read_file "menu.html" |> parse in
  (* TODO div[class^='section-menu-idx-'] *)
  soup $$ "div[data-id-product]" |>
  to_list |>
  List.map (fun p ->
    let product_type = p |> R.attribute "data-product-type" in
    let is_main = p |> R.attribute "data-is-main" = "1" in
    let id = p |> R.attribute "data-id-product" |> int_of_string in
    let picture = p $ "img" |> R.attribute "data-original" in
    let chef =
      match p $ "div.chef-name" |> R.leaf_text with
      | "FoodChéri" -> None
      | x -> Some x
    in
    let title = p $ "h2.title" |> R.leaf_text in
    let tags = p $$ "ul.product-tags li" |> to_list |> List.map R.leaf_text in
    let price =
      p $ "span.price-value" |>
      trimmed_texts |> String.concat "" |>
      Re.replace_string ~by:"." (Re.compile (Re.char ',')) |>
      float_of_string
    in
    {product_type; is_main; id; picture; chef; title; tags; price}
  ) |> Lwt.return

let check_stock delivery products =
  (* TODO take products, not just IDs *)
  let params =
    let l = products |> List.map string_of_int |> String.concat "," in
    [
      "idProductPublicationList", ["[null,null,[" ^ l ^ "]]"];
      "deliveryDate", [delivery]
    ]
  in
  (* TODO check the `code` field? *)
  Http.call `POST "/ajaxCheckStock" params
  >>= Utils.tee (* FIXME fails since content=[], do we need a cookie? *)
  >|= Yojson.Safe.from_string
  >|= Yojson.Safe.Util.member "content"
  >|= Yojson.Safe.Util.to_assoc
  >>= Lwt_list.filter_map_p @@ fun (k, v) ->
    if Yojson.Safe.Util.to_string v = "STOCK" then
      Lwt.return None
    else
      Lwt.return (Some (int_of_string k))
