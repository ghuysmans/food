let price p =
  Tyxml.Html.pcdata (Printf.sprintf "%.2f€" p)

let page page_title content =
  let open Tyxml.Html in
  let title = title (pcdata page_title) in
  let h1 = h1 [pcdata page_title] in
  let css = "label {display: block}" in
  let utf8 = meta ~a:[a_charset "utf8"] () in
  html (head title [utf8; style [pcdata css]]) (body (h1 :: content))
