(* file: main.ml *)
(* Assumes the parser file is "rtcalc.mly" and the lexer file is "lexer.mll". *)



let main () =
  while true do
   try (Calc.input Lexer.token (Lexing.from_string (read_line ()^"\n")) |> Deriveur.print_struct;Printf.printf "\n") with
   |  Stdlib.Parsing.Parse_error -> Printf.printf "Error\n%!"
   | Deriveur.Empty_input -> Printf.printf "Veuillez renseigner une fonction :\n"
  done
  let _ = Printexc.print main ();;
  