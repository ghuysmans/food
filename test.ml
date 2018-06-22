open Eo
open Relalg
open Template

let sp_menu t =
  List.filter (fun (t', _, _) -> t = t') menu |>
  List.map (fun (_, n, p) -> (n, p)) |>
  List.sort (fun (n, _) (n', _) -> compare n n')

let title_of_t ((p, _, _): t) =
  match p with
  | `Potage -> "Potages"
  | `Entree _ -> "Entrées"
  | `Entree_vapeur _ -> "Entrées à la vapeur"
  | `Plat_riz | `Beignets_sad | `Tofu_saute -> "Plats accompagnés de riz blanc"
  | `Riz_saute -> "Riz sautés"
  | `Nouilles_sautees -> "Nouilles sautées"

let board l selected = Tyxml.Html.(
  let rec f (acc, t) l =
    match l with
    | [] -> List.rev acc
    | ((_, _, pr) as x) :: r ->
      let str = (string_of_t x) in
      let row = tr [
        td [price pr];
        td [label [
          Utils.checkbox str (List.mem str selected);
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


let render_form selected =
  let j =
    full_outer_join_u (sp_menu `Riz_saute, sp_menu `Nouilles_sautees) |>
    (*
    Template.ojoin ~check:"✓" ~none:"✗" ("Riz", "Nouilles") "... sauté(es)"
    *)
    Template.ojoin ~check:"✅" ~none:"❎" ("Riz", "Nouilles") "... sauté(es)"
  in
  let m = board menu selected in
  Tyxml.Html.[
    form ~a:[a_method `Post] [
      Template.who "Qui es-tu ? ";
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

let render_list () =
  let items = List.map (fun u -> u, Orders.fetch u) (Orders.who ()) in
  let counts = snd (List.split items) |> List.flatten |> Utils.count_dup in
  Tyxml.Html.[
    h2 [pcdata "Par personne"];
    ul (items |> List.map (fun (u, i) ->
      li [pcdata (u ^ " : " ^ String.concat ", " i)]));
    h2 [pcdata "Par plat"];
    ul (counts |> List.map (fun (i, c) ->
      li [pcdata (Printf.sprintf "%s : %d" i c)]));
  ]


let () = Lwt_main.run (My_server.create (`TCP (`Port 8000)) (fun h p uri ->
  let re = Tyre.(route [
    (start *> str "/" *> stop --> fun _ ->
      match p with
      | None ->
        let data =
          match Uri.get_query_param uri "who" with
          | None -> []
          | Some u -> Orders.fetch u
        in
        `Ok ("Extrême Orient", render_form data)
      | Some p ->
        let sel =
          Uri.remove_query_param p "who" |> Uri.query |>
          List.split |> fst
        in
        match Uri.get_query_param p "who" with
        | Some who when List.mem who (Orders.who ()) ->
          Orders.update who sel;
          `Redirect "/all"
        | _ ->
          `Bad_request);
    (start *> str "/all" *> stop --> fun _ ->
      `Ok ("Réservations", render_list ()));
  ]) in
  let h = Cohttp.Header.init () in
  match Tyre.exec re (Uri.path uri) with
  | Ok (`Ok (t, b)) ->
    let page = Template.page t b in
    My_server.respond `OK h page
  | Ok `Bad_request ->
    let page = Template.page "Bad request" [] in
    My_server.respond `Bad_request h page
  | Ok (`Redirect u) ->
    My_server.respond_redirect uri h u
  | Error _ ->
    let page = Template.page "Not found" [Tyxml.Html.pcdata (Uri.path uri)] in
    My_server.respond `Not_found h page
))
