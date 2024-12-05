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

type literal = { b : bool; p : relName; tms : tm list }
type clause = literal list

exception InvalidForm of string

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

let rec substT x t = function
  | Var y -> if x = y then t else Var y
  | Fun (f, ts) -> Fun (f, List.map (substT x t) ts)

let rec subst x t = function
  | Not f -> Not (subst x t f)
  | Cv (c, f1, f2) -> Cv (c, subst x t f1, subst x t f2)
  | Qf (q, y, f) -> if x = y then Qf (q, y, f) else Qf (q, y, subst x t f)
  | Rel (p, ts) -> Rel (p, List.map (substT x t) ts)

let substL x t l = { l with tms = List.map (substT x t) l.tms }

let rec pnf = function
  | Rel (p, ts) -> Rel (p, ts)
  | Not f -> (
      match f with
      | Rel (p, ts) -> Not (Rel (p, ts))
      | _ -> raise (InvalidForm "expected nnf in pnf"))
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
      match f with
      | Rel (p, ts) -> Not (Rel (p, ts))
      | _ -> raise (InvalidForm "expected nnf in cnf"))
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

let rec disj_clause cl = function
  | Rel (p, ts) -> { b = true; p; tms = ts } :: cl
  | Not (Rel (p, ts)) -> { b = false; p; tms = ts } :: cl
  | Cv (Or, f1, f2) -> disj_clause (disj_clause cl f1) f2
  | _ -> raise (InvalidForm "expected cnf in disj_clause")

let rec conj_clauses xs cls = function
  | Qf (Forall, x, f) -> conj_clauses (x :: xs) cls f
  | Cv (And, f1, f2) -> conj_clauses xs (conj_clauses xs cls f1) f2
  | f ->
      let f = List.fold_left (fun f x -> subst x (Var (freshVar x)) f) f xs in
      disj_clause [] f :: cls

let clausal_form : t -> clause list = conj_clauses [] []

let print_literal (l : literal) =
  if not l.b then print_string "~";
  print_string l.p;
  print_string "(";
  print_tms l.tms

let rec print_clause (c : clause) =
  match c with
  | [] -> print_string "False"
  | [ l ] -> print_literal l
  | l :: c ->
      print_literal l;
      print_string " || ";
      print_clause c

type substMap = (var * tm) list

let ( let* ) = Option.bind

let rec unify_tm (c : substMap) t1 t2 : substMap option =
  match (t1, t2) with
  | Var x, Var y -> (
      match (List.assoc_opt x c, List.assoc_opt y c) with
      | Some t1', Some t2' -> unify_tm c t1' t2'
      | Some t1', None -> Some ((y, t1') :: c)
      | None, Some t2' -> Some ((x, t2') :: c)
      | None, None -> Some ((x, Var y) :: c))
  | Var x, Fun (f, ts) | Fun (f, ts), Var x -> (
      match List.assoc_opt x c with
      | Some t' -> unify_tm c (Fun (f, ts)) t'
      | None -> Some ((x, Fun (f, ts)) :: c))
  | Fun (f1, ts1), Fun (f2, ts2) ->
      if f1 <> f2 then None
      else
        List.fold_left2
          (fun c t1 t2 ->
            let* c = c in
            unify_tm c t1 t2)
          (Some c) ts1 ts2

let unify l1 l2 =
  if l1.p <> l2.p then None
  else
    List.fold_left2
      (fun c t1 t2 ->
        let* c = c in
        unify_tm c t1 t2)
      (Some []) l1.tms l2.tms

let applySubsts (m : substMap) (l : literal) =
  List.fold_left (fun l (x, t) -> substL x t l) l m

let rec print_substs (c : substMap) =
  match c with
  | [] -> print_string "no substs"
  | [ (x, t) ] ->
      print_string x;
      print_string "|->";
      print_tm t
  | (x, t) :: c' ->
      print_string x;
      print_string "|->";
      print_tm t;
      print_string ",";
      print_substs c'

type stepResult = { c : clause; l1 : literal; l2 : literal; m : substMap }

let factor (co : clause) : stepResult option =
  let rec loop1 c =
    match c with
    | [] -> None
    | l1 :: c' -> (
        let rec loop2 d =
          match d with
          | [] -> None
          | l2 :: d' ->
              if l1.b = l2.b then
                match unify l1 l2 with
                | None -> loop2 d'
                | Some m ->
                    Some
                      {
                        c =
                          List.map (applySubsts m)
                            (l1 :: List.filter (fun l -> l <> l1 && l <> l2) co);
                        l1;
                        l2;
                        m;
                      }
              else loop2 d'
        in
        match loop2 c' with None -> loop1 c' | Some r -> Some r)
  in
  loop1 co

let search_factor (cs : clause list) : (clause * stepResult) option =
  let rec loop = function
    | [] -> None
    | c :: cs' -> (
        match factor c with
        | None -> loop cs'
        | Some r -> if List.mem r.c cs then loop cs' else Some (c, r))
  in
  loop cs

let resolve (co1 : clause) (co2 : clause) : stepResult option =
  let rec loop1 c =
    match c with
    | [] -> None
    | l1 :: c' -> (
        let rec loop2 d =
          match d with
          | [] -> None
          | l2 :: d' ->
              if l1.b <> l2.b then
                match unify l1 l2 with
                | None -> loop2 d'
                | Some m ->
                    Some
                      {
                        c =
                          List.map (applySubsts m)
                            (List.filter (( <> ) l1) co1
                            @ List.filter (( <> ) l2) co2);
                        l1;
                        l2;
                        m;
                      }
              else loop2 d'
        in
        match loop2 co2 with None -> loop1 c' | Some r -> Some r)
  in
  loop1 co1

let search_resolve (cs : clause list) : (clause * clause * stepResult) option =
  let rec loop1 = function
    | [] -> None
    | c :: cs' -> (
        let rec loop2 = function
          | [] -> None
          | d :: ds' -> (
              match resolve c d with
              | None -> loop2 ds'
              | Some r -> if List.mem r.c cs then loop2 ds' else Some (c, d, r))
        in
        match loop2 cs' with None -> loop1 cs' | Some r -> Some r)
  in
  loop1 cs

let print_factor (c : clause) (f : stepResult) =
  print_string "Factoring: ";
  print_clause c;
  print_string " | ";
  print_literal f.l1;
  print_string " and ";
  print_literal f.l2;
  print_string " unify with ";
  print_substs f.m;
  print_string " to ";
  print_clause f.c

let print_resolve (c1 : clause) (c2 : clause) (f : stepResult) =
  print_string "Resolving: ";
  print_clause c1;
  print_string " | ";
  print_clause c2;
  print_string " | ";
  print_literal f.l1;
  print_string " and ";
  print_literal f.l2;
  print_string " unify with ";
  print_substs f.m;
  print_string " to ";
  print_clause f.c
