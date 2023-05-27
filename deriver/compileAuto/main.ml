(* file: main.ml *)
(* Assumes the parser file is "rtcalc.mly" and the lexer file is "lexer.mll". *)
let main () =
  try
  let lexbuf = 
    Lexing.from_channel stdin in
  while true do
   try Calc.input Lexer.token lexbuf with
   |  Stdlib.Parsing.Parse_error -> Printf.printf "Error\n%!"
  done
  with End_of_file -> exit 0
  let _ = Printexc.print main ()
  