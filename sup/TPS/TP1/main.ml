let rec n_etoiles = function
| n when n<0 -> failwith "Invalid argument"
| 0 -> print_char '\n'; ()
| n -> print_char '*'; n_etoiles (n-1);;

let triangle_bas n =
   let rec aux = function
   | k when k<=n -> n_etoiles k; aux (k+1)
   | _ -> ()
   in aux 1;;

let rec triangle_haut = function
| n when n<0 -> failwith "Invalid argument"
| 0 -> ()
| n -> n_etoiles n; triangle_haut (n-1);;

let rec fibonacci = function
| n when n<0 -> failwith "Invalid argument"
| 0 -> 0
| 1 -> 1
| 2 -> 1
| n -> fibonacci (n-1) + fibonacci (n-2)

let rec stern = function
| n when n<0 -> failwith "Invalid argument"
| 0 -> 0
| 1 -> 1
| n when n mod 2 = 0 -> stern (n/2)
| n -> stern (n/2) + stern (n/2+1)

let calkin_wilf n = (n |> stern |> float_of_int) /. (n+1 |> stern |> float_of_int );;

let rec nb1 = function
| n when n<0 -> failwith "Invalid argument"
| 0 -> if (calkin_wilf 0) <= 1. then 1 else 0
| n -> if (calkin_wilf n) <= 1. then 1 + nb1 (n-1) else nb1 (n-1);;

let rec catalan = function
| n when n<0 -> failwith "Invalid argument"
| 0 -> 1
| n -> let rec aux = function
   | 0 -> catalan (n-1)
   | k -> (catalan k)*(catalan (n-1-k)) + aux (k-1)
   in aux (n-1);;

let rec est_palindrome = function
| "" -> true
| str -> if String.length str=1 then true 
	 else if str.[0]=str.[(String.length str)-1]
	 then est_palindrome (String.sub str 1 ((String.length str)-2)) else false;;

let (++) a b= a ^ b;; 

let retourne str =
   let rec aux str = function
   | "" -> str
   | inputStr -> aux 
	(str ++ (String.sub inputStr ((String.length inputStr)-1) 1)) 
	(String.sub inputStr 0 ((String.length inputStr)-1))
in aux "" str;;

let fonction n = n |> string_of_int |> retourne |> int_of_string;;

let suivant u_n = u_n + fonction u_n;;

let nb_iter u_0 =
  let rec aux n = function
  | u_n when est_palindrome (u_n |> string_of_int) -> n
  | u_n -> aux (n+1) (suivant u_n)
in aux 0 u_0;;


let print_bool = function
| true -> print_endline "true"
| _ -> print_endline "false";;

let soc c = String.make 1 c;;

let rec ajoute0 str = function
| n when n<=0 -> str
| n -> ajoute0 ("0"++str) (n-1);;

let rec somme_str str1 str2=
   if String.length str1 > String.length str2 then somme_str str2 str1 else
   let rec aux acc retenue str2 = function
   | "" -> if retenue<>0 then (retenue |> string_of_int)++acc  else acc
   | str1 ->aux 
	(	((((soc str1.[(String.length str1)-1] |> int_of_string)  + 
		(soc str2.[(String.length str2)-1] |> int_of_string) + retenue) mod 10) 
			|> string_of_int)
	 ++ acc)
	(((soc str1.[(String.length str1)-1] |> int_of_string) +
          (soc str1.[(String.length str2)-1] |> int_of_string) + retenue) / 10)
	(String.sub str2 0 ((String.length str2) - 1))
	(String.sub str1 0 ((String.length str1) - 1))
in aux "" 0 str2 (ajoute0 str1 ((String.length str2)-(String.length str1)));;

let print_gs () = print_string "\027[42m  \027[0m";;

let trace n f = 
  if n<0 then failwith "Invalid argument" else
  let rec aux = function
  |(k,j) when (k,j)=(n,n) -> print_char '\n'
  |(k,j) when (k,j)=(k,n) -> print_char '\n'; aux (k+1,0)
  |(k,j) -> if f k j then (print_gs (); aux (k,j+1)) else (print_char ' '; aux (k,j+1))
  in aux (0,0)
;;

let pow a n =
  let rec aux acc = function
  | k when k<0 -> failwith "Invalid argument"
  | 0 -> acc
  | k -> aux (acc*a) (k-1)
  in aux 1 n;; 

let f_demo_1 i j = (i+j) mod 4=2;;

let f_demo_2 i j = abs (i - 10/2) <= abs (j);;

let () = if Array.length Sys.argv>=3 then match Sys.argv.(1) with
| "n_etoiles" -> n_etoiles (int_of_string (Sys.argv.(2)))
| "triangle_bas" -> triangle_bas (int_of_string (Sys.argv.(2)))
| "triangle_haut" -> triangle_haut (int_of_string (Sys.argv.(2)))
| "fibonacci" -> print_int (fibonacci (int_of_string (Sys.argv.(2)))); print_char '\n'
| "stern" -> print_int (stern  (int_of_string (Sys.argv.(2)))); print_char '\n';
| "calkin_wilf" -> print_float (calkin_wilf  (int_of_string (Sys.argv.(2)))); print_char '\n'
| "nb1" -> print_int (nb1 (int_of_string (Sys.argv.(2)))); print_char '\n'
| "catalan" -> print_int (catalan (int_of_string (Sys.argv.(2)))); print_char '\n'
| "est_palindrome" -> print_bool (est_palindrome (Sys.argv.(2)))
| "retourne" -> print_endline (retourne (Sys.argv.(2)))
| "nb_iter" -> print_endline (Sys.argv.(2) |> int_of_string |> nb_iter |> string_of_int)
| "somme_str" when Array.length Sys.argv=4 -> print_endline (somme_str Sys.argv.(2) Sys.argv.(3)) 
| "pow" when Array.length Sys.argv=4 -> print_endline (pow (Sys.argv.(2) |> int_of_string) (Sys.argv.(3) |> int_of_string) |> string_of_int)
| "trace_demo_1" -> trace (Sys.argv.(2) |> int_of_string) f_demo_1
| "trace_demo_2" -> trace (Sys.argv.(2) |> int_of_string) f_demo_2 
| _ -> print_endline "Nothing to do";;
