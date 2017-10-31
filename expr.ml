type 'a v =
  (* Values *)
  | String: string -> string v
  | Int: int -> int v
  | Float: float -> float v
  | List: 'a v list -> 'a list v
  (* Operations *)
  | Plus: 'a v * 'a v -> 'a v
  | Times: 'a v * 'a v -> 'a v

let rec eval: type a. a v -> a = function
  | String s -> s
  | Int i -> i
  | Float f -> f
  | List l -> List.map eval l
  | Plus (String s, String s') -> s ^ s'
  | Plus (Int i, Int i') -> i + i'
  | Plus (List l, List l') -> List.map eval l @ List.map eval l'
  | Plus (x, y) -> eval (Plus (eval x, eval y))
  (* ... *)
