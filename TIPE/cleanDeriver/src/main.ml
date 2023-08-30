(* file: main.ml *)
(* Assumes the parser file is "rtcalc.mly" and the lexer file is "lexer.mll". *)


let main () =
  while true do
   try let exp =(Calc.input Lexer.token (Lexing.from_string (read_line ()^"\n"))) in
   exp |> Deriveur.print_struct;Printf.printf "\n"; exp |> Deriveur.print_exp_latex; Printf.printf "\n" with
   | Stdlib.Parsing.Parse_error -> Printf.printf "Error\n%!"
  done
  let _ = Printexc.print main ();;
  