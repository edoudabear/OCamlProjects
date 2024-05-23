(* file: main.ml *)

let show exp= Deriveur.print_struct exp;Printf.printf "\n"; exp |> Deriveur.print_exp_latex; Printf.printf "\n";;


let get_exp input=(Calc.input Lexer.token (Lexing.from_string (input^"\n")))

let check i0=
   let user_input=Array.sub Sys.argv i0 (Array.length Sys.argv-i0) |> Array.fold_left (^)  "" in
   try let exp =(Calc.input Lexer.token (Lexing.from_string (user_input^"\n"))) in
   show exp with
   | Stdlib.Parsing.Parse_error -> Printf.printf "Error\n%!";;

let comparison a b = let exp_a=a |> get_exp and exp_b=b |> get_exp in (*show exp_a; show exp_b;*) Canoniseur.compare_eq exp_a exp_b;;

let test_entree_utilisateur computer_query user_input= 
    let exp_a=user_input |> get_exp and exp_b=computer_query |> get_exp |> Deriveur.deriveur in Canoniseur.compare_eq exp_a exp_b;;

let main () =
   if Array.length Sys.argv = 1 then Deriveur.Leaf_e (false,0,1) |> show else
   match Sys.argv.(1) with
   | "check" -> check 2
   | "compare" when Sys.argv |> Array.length < 4 -> Printf.printf "Missing comparison function";
   | "compare" -> let a,b=Sys.argv.(2),Sys.argv.(3) in exit (comparison a b);
   | "test_entree_utilisateur" -> let a,b=Sys.argv.(2),Sys.argv.(3) in exit (test_entree_utilisateur a b);
   | a -> check 1;;

let _ = Printexc.print main ();;
  