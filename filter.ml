(* write a function to provide some introspection, then use it both in parse
 * (which has to know types...) and test (which uses values). The current
 * implementation forces us to parse the expression for each record, yuck! *)


(* Values *)
type 'a v =
  | String: string -> string v
  | Int: int -> int v
  | Float: float -> float v
  | List: 'a v list -> 'a list v

(* Filter expression *)
type t =
  | Lt: 'a v * 'a v -> t
  | Eq: 'a v * 'a v -> t
  | Gt: 'a v * 'a v -> t
  | And: t list -> t
  | Or: t list -> t
  | In: 'a v * 'a list v -> t

let rec eval: type a. a v -> a = function
  | String s -> s
  | Int i -> i
  | Float f -> f
  | List l -> List.map eval l

let rec test = function
  | Lt (x, x') -> eval x < eval x'
  | Eq (x, x') -> eval x = eval x'
  | Gt (x, x') -> eval x > eval x'
  | And l -> List.map test l |> List.fold_left (&&) true
  | Or l -> List.map test l |> List.fold_left (||) false
  | In (x, l) -> List.mem (eval x) (eval l)

(*
let rec to_string: type a. a t -> string = function
  | String s -> "\"" ^ String.escaped s ^ "\""
  | Int i -> string_of_int i
  | Eq (x, x') -> to_string x ^ " = " ^ to_string x'
  | And l -> "(" ^ String.concat " && " (List.map to_string l) ^ ")"
  | Or l -> "(" ^ String.concat " || " (List.map to_string l) ^ ")"
  | In (x, l) -> to_string x ^ " in " ^ to_string l
  | List l -> "[" ^ String.concat "; " (List.map to_string l) ^ "]"
*)

let parse_string s =
  String s (* FIXME quotes *)

let parse_int s =
  Int (int_of_string s)

let parse_float s =
  Float (float_of_string s)

(*
let parse_list f s =
  List (List.map f (Str.split (Str.regexp_string ",") s))
*)

type vv =
  | VV: 'a v * 'a v -> vv
let read_vv field const =
  match field with
  | "name" -> VV (String "Bob", parse_string const)
  | "arms" -> VV (Int 2, parse_int const)
  | "age" -> VV (Float 19.7, parse_float const)
  | _ -> failwith @@ "unknown field " ^ field

type vl =
  | VL: 'a v * 'a list v -> vl
let read_vl value field =
  match field with
  | "friends" -> VL (parse_string value, List [String "Alice"])
  | "languages" -> VL (parse_string value, List [String "en"; String "fr"])
  | "cards" -> VL (parse_int value, List [Int 1; Int 3; Int 5])
  | _ -> failwith @@ "unknown field " ^ field

let parse left op const =
  match op with
  | "<" -> let VV (left, const) = read_vv left const in Lt (left, const)
  | "=" -> let VV (left, const) = read_vv left const in Eq (left, const)
  | ">" -> let VV (left, const) = read_vv left const in Gt (left, const)
  | "in" -> let VL (left, const) = read_vl left const in In (left, const)
  | _ -> failwith @@ "unknown operator " ^ op
