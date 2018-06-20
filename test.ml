open Eo
open Relalg
open Template

let sp_menu t =
  List.filter (fun (t', _, _) -> t = t') menu |>
  List.map (fun (_, n, p) -> (n, p)) |>
  List.sort (fun (n, _) (n', _) -> compare n n')

let ojoin (l, r) s j = Tyxml.Html.(
  let thead = thead [tr [th [pcdata l]; th [pcdata r]; th [pcdata s]]] in
  table ~a:[a_class ["synth"]] ~thead (
    j |> List.map (fun (n, r) ->
      match r with
      | Left l -> tr [td [price l]; td []; td [pcdata n]]
      | Right r -> tr [td []; td [price r]; td [pcdata n]]
      | Both (l, r) -> tr [td [price l]; td [price r]; td [pcdata n]]
    )
  )
)

let title_of_t ((p, _, _): t) =
  match p with
  | `Potage -> "Potages"
  | `Entree _ -> "Entrées"
  | `Entree_vapeur _ -> "Entrées à la vapeur"
  | `Plat_riz | `Beignets_sad | `Tofu_saute -> "Plats accompagnés de riz blanc"
  | `Riz_saute -> "Riz sautés"
  | `Nouilles_sautees -> "Nouilles sautées"

let board l = Tyxml.Html.(
  let rec f (acc, t) l =
    match l with
    | [] -> List.rev acc
    | ((_, _, pr) as x) :: r ->
      let str = (string_of_t x) in
      let row = tr [
        td [price pr];
        td [label [
          input ~a:[a_input_type `Checkbox; a_name str; a_value "1"] ();
          pcdata str;
        ]];
      ] in
      let t' = title_of_t x in
      if t = t' then
        f (row :: acc, t) r
      else
        f (row :: tr [td ~a:[a_colspan 2] [h2 [pcdata t']]] :: acc, t') r
  in
  table (f ([], "") l)
)

let who lbl = Tyxml.Html.(
  (*
  Tyxml.Html.(label [pcdata "Nom : "; input ~a:[a_name "who"; a_required ()] ()])
  *)
  label [
    pcdata lbl;
    select ~a:[a_name "who"; a_required ()]
      (List.map (fun x -> option (pcdata x)) ("" :: Config.who))
  ]
)


let render =
  let j =
    full_outer_join_u (sp_menu `Riz_saute, sp_menu `Nouilles_sautees) |>
    ojoin ("Riz", "Nouilles") "... sauté(es)"
  in
  let m = board menu in
  Tyxml.Html.[
    form [
      who "Qui es-tu ? ";
      p [pcdata "Coche ce que tu veux et clique sur « Envoyer » tout en bas."];
      m;
      h2 [pcdata "Tableau comparatif"];
      j;
      p [
        pcdata "Au cas où ça ne serait pas clair, ce tableau a été ";
        a ~a:[a_href "https://github.com/ghuysmans/food/blob/master/relalg.ml"] [
          pcdata "généré"
        ];
        pcdata ", il n'est pas statique... ";
      ];
      a ~a:[a_href "https://xkcd.com/1319/"] [
        img ~src:"https://imgs.xkcd.com/comics/automation.png" ~alt:"xkcd"
            ~a:[a_title "De l'abus ? Nooooon, pas du tout !"] ();
      ];
      hr ();
      button [pcdata "Envoyer"];
    ];
  ]


let () =
  let fmt = Format.formatter_of_out_channel stdout in
  let title = "Extrême Orient" in
  let page = Template.page title render in
  Tyxml.Html.pp () fmt page;
  Format.pp_print_flush fmt ();
