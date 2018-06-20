(*
let rec skip_until t = function
  | [] -> []
  | ((t', _, _) :: r) as l when t = t' -> l
  | _ :: r -> skip_until t r
*)

let select t l =
  List.filter (fun (t', _) -> t = t') l

let rec join_u ?(acc=[]) = function
  | [], _ | _, [] ->
    List.rev acc
  | ((n, p) :: r as l), ((n', p') :: r' as l') ->
    if n = n' then
      join_u ~acc:((n, (p, p')) :: acc) (r, r')
    else if n < n' then
      join_u ~acc (r, l')
    else
      join_u ~acc (l, r')

let rec left_join_u ?(acc=[]) = function
  | [], _ ->
    List.rev acc
  | (n, p) :: r, [] ->
    left_join_u ~acc:((n, (p, None)) :: acc) (r, [])
  | ((n, p) :: r as l), ((n', p') :: r' as l') ->
    if n = n' then
      left_join_u ~acc:((n, (p, Some p')) :: acc) (r, r')
    else if n < n' then
      left_join_u ~acc:((n, (p, None)) :: acc) (r, l')
    else
      left_join_u ~acc (* skip *) (l, r')

type ('a, 'b) outer_join =
  | Left of 'a
  | Right of 'b
  | Both of 'a * 'b

let rec full_outer_join_u ?(acc=[]) = function
  | [], [] ->
    List.rev acc
  | [], (n', p') :: r' ->
    full_outer_join_u ~acc:((n', Right p') :: acc) ([], r')
  | (n, p) :: r, [] ->
    full_outer_join_u ~acc:((n, Left p) :: acc) (r, [])
  | ((n, p) :: r as l), ((n', p') :: r' as l') ->
    if n = n' then
      full_outer_join_u ~acc:((n, Both (p, p')) :: acc) (r, r')
    else if n < n' then
      full_outer_join_u ~acc:((n, Left p) :: acc) (r, l')
    else
      full_outer_join_u ~acc:((n', Right p') :: acc) (l, r')
