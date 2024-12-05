open Folprover

let allclauses = ref []

let () =
  let lexbuf = Lexing.from_string Sys.argv.(1) in
  let f = Parser.goal Lexer.token lexbuf in
  Fol.print f;
  print_newline ();
  let neg = Fol.Not f in
  Fol.print neg;
  print_newline ();
  let nnf = Fol.nnf neg in
  Fol.print nnf;
  print_newline ();
  let pnf = Fol.pnf nnf in
  Fol.print pnf;
  print_newline ();
  let skm = Fol.skolemize pnf in
  Fol.print skm;
  print_newline ();
  let cnf = Fol.cnf skm in
  Fol.print cnf;
  print_newline ();
  let cls = Fol.clausal_form cnf in
  List.iter
    (fun c ->
      Fol.print_clause c;
      print_string "; ")
    cls;
  print_newline ();
  allclauses := cls;
  let rec step () =
    match Fol.search_resolve !allclauses with
    | Some (c1, c2, r) ->
        Fol.print_resolve c1 c2 r;
        print_newline ();
        if r.c = [] then
          print_endline "Found a contradiction! Original formula is valid."
        else (
          allclauses := r.c :: !allclauses;
          step ())
    | None -> (
        match Fol.search_factor !allclauses with
        | Some (c, r) ->
            Fol.print_factor c r;
            print_newline ();
            allclauses := r.c :: !allclauses;
            step ()
        | None ->
            print_endline "Cannot take a step! Original formula is invalid.")
  in
  step ()
