open Folprover

let () =
  let lexbuf = Lexing.from_string Sys.argv.(1) in
  let f = Parser.goal Lexer.token lexbuf in
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
    cls
