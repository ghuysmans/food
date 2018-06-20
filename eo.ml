type plus = [
    `Potage
  | `Entree of int
  | `Entree_vapeur of int
  | `Plat_riz
  | `Riz_saute
  | `Nouilles_sautees
  | `Beignets_sad (* FIXME riz ? *)
  | `Tofu_saute
]

type t = plus * string * float

type cat =
  | Potage
  | Entree
  | Plat

let cat_of_t ((p, _, _): t) =
  match p with
  | `Potage -> Potage
  | `Entree _ -> Entree
  | `Entree_vapeur _ -> Entree
  | `Plat_riz -> Plat
  | `Riz_saute -> Plat
  | `Nouilles_sautees -> Plat
  | `Beignets_sad -> Plat
  | `Tofu_saute -> Plat

let string_of_cat = function
  | Potage -> "POTAGE"
  | Entree -> "ENTRÉE"
  | Plat -> "PLAT"

let string_of_t ((p, n, _): t) =
  match p with
  | `Potage -> n
  | `Entree c | `Entree_vapeur c when c > 1 -> Printf.sprintf "%s (%d pièces)" n c
  | `Entree c | `Entree_vapeur c -> n
  | `Plat_riz -> n
  | `Beignets_sad -> "Beignets " ^ n ^ " sauce aigre-douce"
  | `Riz_saute -> "Riz sauté " ^ n
  | `Nouilles_sautees -> "Nouilles sautées " ^ n
  | `Tofu_saute -> "Tofu sauté " ^ n

let menu : t list = [
  `Potage, "Potage piquant", 2.5;
  `Potage, "Crème nid d'hirondelle", 2.5;
  `Potage, "Potage bambou et poulet", 2.5;
  `Potage, "Potage chinois", 3.;
  `Potage, "Soupe au wantan soupe", 3.5;

  `Entree 3, "Brochettes de poulet", 3.;
  `Entree 5, "Nems", 3.5;
  `Entree 4, "Nems aux légumes", 3.;
  `Entree 12, "Beignets de poulet", 3.5;
  `Entree 4, "Beignets de scampis", 3.;
  `Entree 10, "Beignets de calamar", 3.;
  `Entree 6, "Bouchées de porc", 4.;
  `Entree 6, "Wantans frits", 3.;
  `Entree 5, "Croquettes de poulet", 3.5;
  `Entree 4, "Loempias aux scampis", 3.;
  `Entree 4, "Loempias au poulet et soja", 3.;
  `Entree 4, "Triangles de poulet au curry", 3.;
  `Entree 4, "Assortiment de croquettes", 3.;
  `Entree 12, "Assortiment de beignets", 4.5;
  `Entree 6, "Assiette croustillante (+ salade vietnamienne)", 4.5;
  `Entree 1, "Salade vietnamienne", 3.;

  `Entree_vapeur 6, "Ha Cao", 4.;
  `Entree_vapeur 6, "Xiu Mai", 4.;
  `Entree_vapeur 2 (* ? *), "Crêpes de riz vietnamiennes garnies à la vapeur", 6.;
  `Entree_vapeur 5, "Raviolis de riz aux crevettes", 4.;
  `Entree_vapeur 5, "Petits croissants de crevettes", 4.;
  `Entree_vapeur 6, "Assortiment à la vapeur", 5.;

  `Plat_riz, "Poulet impérial", 5.5;
  `Plat_riz, "Poulet aux légumes", 5.5;
  `Plat_riz, "Poulet sauce aigre-douce", 5.5;
  `Plat_riz, "Poulet sauce piquante", 5.5;
  `Plat_riz, "Poulet saté", 5.5;
  `Plat_riz, "Poulet au curry thaï", 6.;
  `Plat_riz, "Poulet au curry rouge (piquant)", 6.;
  `Plat_riz, "Poulet caramélisé", 6.;
  `Plat_riz, "Poulet aux 5 parfums", 6.;
  `Plat_riz, "Poulet sauté maison", 6.;
  `Plat_riz, "Poulet à l'orange", 7.;
  `Beignets_sad, "de poulet", 6.;

  `Beignets_sad, "de porc", 6.;
  `Beignets_sad, "de scampis", 8.;
  `Beignets_sad, "(assortiment)", 7.;

  `Plat_riz, "Porc sauce piquante", 6.;
  `Plat_riz, "Porc caramélisé", 6.;
  `Plat_riz, "Porc sauce citronnelle", 6.;

  `Plat_riz, "Bœuf 5 parfums", 6.5;
  `Plat_riz, "Bœuf champignons bambous", 6.5;
  `Plat_riz, "Bœuf à l'ail piquant", 6.5;
  `Plat_riz, "Bœuf caramélisé", 6.5;
  `Plat_riz, "Bœuf sauté maison", 7.;

  `Plat_riz, "Canard laqué", 8.5;
  `Plat_riz, "Canard Pékin Crêpe", 9.;
  `Plat_riz, "Canard caramélisé", 9.;
  `Plat_riz, "Canard aux ananas", 8.5;
  `Plat_riz, "Canard à l'ail piquant", 8.5;
  `Plat_riz, "Canard au curry et lait de coco", 8.5;
  `Plat_riz, "Canard laqué à l'orange", 10.;

  `Plat_riz, "Calamars sauce piquante", 6.5;
  `Plat_riz, "Calamars 5 parfums", 7.;

  `Plat_riz, "Scampis impérial", 7.;
  `Plat_riz, "Scampis sauce piquante", 7.;
  `Plat_riz, "Scampis caramélisés", 9.;

  `Riz_saute, "au poulet", 5.5;
  `Riz_saute, "au porc", 6.;
  `Riz_saute, "au bœuf", 6.5;
  `Riz_saute, "au porc laqué", 6.5;
  `Riz_saute, "cantonnais spécial", 6.5;
  `Riz_saute, "aux scampis", 7.;
  `Riz_saute, "aux 5 délices", 8.;
  `Riz_saute, "aux légumes", 3.5;

  `Nouilles_sautees, "au poulet", 5.5;
  `Nouilles_sautees, "au porc", 6.;
  `Nouilles_sautees, "au bœuf", 6.5;
  `Nouilles_sautees, "au porc laqué", 6.5;
  `Nouilles_sautees, "aux scampis", 7.;
  `Nouilles_sautees, "aux 5 délices", 8.;
  `Nouilles_sautees, "au canard", 9.;
  `Nouilles_sautees, "aux légumes", 3.5;
  `Nouilles_sautees, "au tofu", 6.;

  `Tofu_saute, "aux légumes", 7.;
  `Tofu_saute, "à l'ail piquant", 7.;
  `Tofu_saute, "aux champignons et bambous", 7.;
]
