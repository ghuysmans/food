let price p =
  Tyxml.Html.pcdata (Printf.sprintf "%.2f€" p)

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
