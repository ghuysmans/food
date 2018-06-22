open Eo
open Relalg
open Template

let sp_menu t =
  List.filter (fun (t', _, _) -> t = t') menu |>
  List.map (fun (_, n, p) -> (n, p)) |>
  List.sort (fun (n, _) (n', _) -> compare n n')

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
      | Left l -> tr [td [price l]; td none; td [pcdata n]]
      | Right r -> tr [td none; td [price r]; td [pcdata n]]
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

let checkbox name ?(value="1") checked =
  let open Tyxml.Html in
  let a = [a_input_type `Checkbox; a_name name; a_value value] in
  input ~a:(if checked then a_checked () :: a else a) ()

let board l selected = Tyxml.Html.(
  let rec f (acc, t) l =
    match l with
    | [] -> List.rev acc
    | ((_, _, pr) as x) :: r ->
      let str = (string_of_t x) in
      let row = tr [
        td [price pr];
        td [label [
          checkbox str (List.mem str selected);
          pcdata str;
        ]];
      ] in
      let t' = title_of_t x in
      if t = t' then
        f (row :: acc, t) r
      else
        f (row :: tr [td ~a:[a_colspan 2] [h3 [pcdata t']]] :: acc, t') r
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
      (List.map (fun x -> option (pcdata x)) ("" :: Orders.who ()))
  ]
)


let render_form selected =
  let j =
    full_outer_join_u (sp_menu `Riz_saute, sp_menu `Nouilles_sautees) |>
    (*
    ojoin ~check:"✓" ~none:"✗" ("Riz", "Nouilles") "... sauté(es)"
    *)
    ojoin ~check:"✅" ~none:"❎" ("Riz", "Nouilles") "... sauté(es)"
  in
  let m = board menu selected in
  Tyxml.Html.[
    form [
      who "Qui es-tu ? ";
      p [pcdata "Coche ce que tu veux et clique sur « Envoyer » tout en bas."];
      m;
      h2 [pcdata "Pour ceux qui hésitent"];
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
      button ~a:[a_button_type `Reset] [pcdata "Effacer"];
    ];
  ]

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

let render_list () =
  let items = List.map (fun u -> u, Orders.fetch u) (Orders.who ()) in
  let counts = snd (List.split items) |> List.flatten |> count_dup in
  Tyxml.Html.[
    h2 [pcdata "Par personne"];
    ul (items |> List.map (fun (u, i) ->
      li [pcdata (u ^ " : " ^ String.concat ", " i)]));
    h2 [pcdata "Par plat"];
    ul (counts |> List.map (fun (i, c) ->
      li [pcdata (Printf.sprintf "%s : %d" i c)]));
  ]


let () = Lwt_main.run (My_server.create (`TCP (`Port 8000)) (fun h p uri ->
  let open Tyre in
  let re = route [
    (start *> str "/" *> stop --> fun _ ->
      let data =
        match Uri.get_query_param uri "who" with
        | None -> []
        | Some u -> Orders.fetch u
      in
      `Ok ("Extrême Orient", render_form data));
    (start *> str "/all" *> stop --> fun _ ->
      `Ok ("Réservations", render_list ()));
  ] in
  let h = Cohttp.Header.init () in
  match Tyre.(exec re (Uri.path uri)) with
  | Ok (`Ok (t, b)) ->
    let page = Template.page t b in
    My_server.respond `OK h page
  | Ok (`Redirect u) ->
    My_server.respond_redirect uri h u
  | Error _ ->
    let page = Template.page "Not found" [Tyxml.Html.pcdata (Uri.path uri)] in
    My_server.respond `Not_found h page
))
