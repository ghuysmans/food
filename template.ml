let price p =
  Tyxml.Html.pcdata (Printf.sprintf "%.2f€" p)

let ojoin ?check ?(none="") (l, r) s j = Tyxml.Html.(
  let thead = thead [tr [th [pcdata l]; th [pcdata r]; th [pcdata s]]] in
  (*
  let c = col ~a:[a_style "width: 20%"] () in
  let columns = [colgroup [c; c; col ()]] in
  *)
  table ~a:[a_class [if check = None then "synth" else "synth_c"]] ~thead (
    j |> List.map (fun (n, r) ->
      let price p =
        match check with
        | None -> price p
        | Some v -> pcdata v
      in
      let none = [pcdata none] in
      match r with
      | Relalg.Left l -> tr [td [price l]; td none; td [pcdata n]]
      | Relalg.Right r -> tr [td none; td [price r]; td [pcdata n]]
      | Relalg.Both (l, r) -> tr [td [price l]; td [price r]; td [pcdata n]]
    )
  )
)

let who lbl = Tyxml.Html.(
  (*
  Tyxml.Html.(label [pcdata "Nom : "; input ~a:[a_name "who"; a_required ()] ()])
  *)
  label [
    pcdata lbl;
    select ~a:[a_name "who"; a_required ()]
      (List.map (fun x -> option (pcdata x)) ("" :: Orders.who ()))
  ]
)

let page page_title content =
  let open Tyxml.Html in
  let title = title (pcdata page_title) in
  let h1 = h1 [pcdata page_title] in
  let css = {|
label {display: block}
button[type="reset"] {float: right}
table {background-color: lightgrey}
table td:first-child:not([colspan]) {text-align: right}
table.synth td:first-child, table.synth td:nth-child(2) {
  width: 20%;
  text-align: left;
}
table.synth_c td:first-child, table.synth_c td:nth-child(2) {
  width: 20%;
  text-align: center;
}
  |} in
  let utf8 = meta ~a:[a_charset "utf8"] () in
  html (head title [utf8; style [Unsafe.data css]]) (body (h1 :: content))
