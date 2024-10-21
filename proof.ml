let check_proof statement =
  if statement = "A -> B" then "Valid" else "Invalid"

let () =
  let statement = Sys.argv.(1) in
  print_endline (check_proof statement)