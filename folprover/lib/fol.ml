type funName = string
type relName = string
type var = string
type tm = Var of var | Fun of funName * tm list
type connective = And | Or
type quantifier = Forall | Exists

let negCv = function And -> Or | Or -> And
let negQf = function Forall -> Exists | Exists -> Forall

type t =
  | Cv of connective * t * t
  | Not of t
  | Qf of quantifier * var * t
  | Rel of relName * tm list

exception InvalidForm

let rec nnf = function
  | Cv (c, f1, f2) -> Cv (c, nnf f1, nnf f2)
  | Qf (q, x, f) -> Qf (q, x, nnf f)
  | Not f -> (
      match nnf f with
      | Not f -> nnf f
      | Cv (c, f1, f2) -> Cv (negCv c, nnf (Not f1), nnf (Not f2))
      | Qf (q, x, f) -> Qf (negQf q, x, nnf (Not f))
      | Rel (p, ts) -> Not (Rel (p, ts)))
  | Rel (p, ts) -> Rel (p, ts)

let counter = ref 0

let freshVar x =
  let x' = x ^ string_of_int !counter in
  incr counter;
  x'

let subst x t =
  let rec substT = function
    | Var y -> if x = y then t else Var y
    | Fun (f, ts) -> Fun (f, List.map substT ts)
  in
  let rec substF = function
    | Not f -> Not (substF f)
    | Cv (c, f1, f2) -> Cv (c, substF f1, substF f2)
    | Qf (q, y, f) -> if x = y then Qf (q, y, f) else Qf (q, y, substF f)
    | Rel (p, ts) -> Rel (p, List.map substT ts)
  in
  substF

let rec pnf = function
  | Rel (p, ts) -> Rel (p, ts)
  | Not f -> (
      match f with Rel (p, ts) -> Not (Rel (p, ts)) | _ -> raise InvalidForm)
  | Qf (q, x, f) -> Qf (q, x, pnf f)
  | Cv (c, f1, f2) -> (
      match (pnf f1, pnf f2) with
      | Qf (q, x, f1'), f2' ->
          let x' = freshVar x in
          Qf (q, x', pnf (Cv (c, subst x (Var x') f1', f2')))
      | f1', Qf (q, x, f2') ->
          let x' = freshVar x in
          Qf (q, x', pnf (Cv (c, f1', subst x (Var x') f2')))
      | f1', f2' -> Cv (c, f1', f2'))

let skolemize =
  let rec skolemize' xs f =
    match f with
    | Qf (Forall, x, f') -> Qf (Forall, x, skolemize' (Var x :: xs) f')
    | Qf (Exists, x, f') -> skolemize' xs (subst x (Fun (x ^ "_sk", xs)) f')
    | _ -> f
  in
  skolemize' []

let rec cnf = function
  | Rel (p, ts) -> Rel (p, ts)
  | Not f -> (
      match f with Rel (p, ts) -> Not (Rel (p, ts)) | _ -> raise InvalidForm)
  | Qf (q, x, f) -> Qf (q, x, cnf f)
  | Cv (And, f1, f2) -> Cv (And, cnf f1, cnf f2)
  | Cv (Or, f1, f2) -> (
      match (cnf f1, cnf f2) with
      | Cv (And, f11, f12), f2' ->
          Cv (And, cnf (Cv (Or, f11, f2')), cnf (Cv (Or, f12, f2')))
      | f1', Cv (And, f21, f22) ->
          Cv (And, cnf (Cv (Or, f1', f21)), cnf (Cv (Or, f1', f22)))
      | f1', f2' -> Cv (Or, f1', f2'))

let rec print_tms ts =
  match ts with
  | [] -> print_string ")"
  | [ t ] ->
      print_tm t;
      print_string ")"
  | t :: ts ->
      print_tm t;
      print_string ",";
      print_tms ts

and print_tm = function
  | Var x -> print_string x
  | Fun (f, ts) ->
      print_string f;
      print_string "(";
      print_tms ts

let rec print = function
  | Rel (p, ts) ->
      print_string p;
      print_string "(";
      print_tms ts
  | Not f ->
      print_string "~";
      print f
  | Cv (c, f1, f2) ->
      print_string "(";
      print f1;
      print_string (match c with Or -> " || " | And -> " && ");
      print f2;
      print_string ")"
  | Qf (q, x, f) ->
      print_string "(";
      print_string (match q with Forall -> "forall " | Exists -> "exists ");
      print_string x;
      print_string ", ";
      print f;
      print_string ")"
