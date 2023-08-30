let rec est_vide = function
| [] -> true
| _ -> false;;

let rec union lst1 lst2= match lst1,lst2 with
| ([],ens2)-> ens2
| (t::q,ens2) -> let rec isDoublon el = function
                 | [] -> false
                 | h::t when h=el -> true
                 | h::t -> isDoublon el t
                 in if isDoublon t ens2 then union q ens2 else union q (t::ens2)
;;

let () = List.iter print_int (union [1;2;3;4] [3;2;6;7]);;

let inter lst1 lst2 =
    let rec interAux acc = function
    | ([],ens2)-> acc
    | (t::q,ens2) -> let rec isDoublon el = function
                 | [] -> false
                 | h::t when h=el -> true
                 | h::t -> isDoublon el t
                 in if isDoublon t ens2 then interAux (t::acc) (q,ens2) else interAux acc (q,ens2)
    in interAux [] (lst1,lst2)
;;

let diff ens1 ens2 =
   let rec diffAux acc = function
   |([],ens2) -> acc
   |(t::q,ens2) -> let rec isDoublon el = function
                 | [] -> false
                 | h::t when h=el -> true
                 | h::t -> isDoublon el t
                 in if isDoublon t ens2 then diffAux (acc) (q,ens2) else diffAux (t::acc) (q,ens2)
    in diffAux [] (ens1,ens2)
;;

let () = print_string "\n" ;List.iter print_int (inter [1;2;3;4] [3;2;6;7]);;
let () = print_string "\n" ;List.iter print_int (diff [1;2;3;4] [3;2;6;7]);;

let disj ens1 ens2 = match inter ens1 ens2 with
| [] -> true
| _ -> false
;;

let inclus ens1 ens2 = match diff ens1 ens2 with
| [] -> true
| _ -> false
;;

let egaux ens1 ens2 =  diff ens1 ens2 = [] && diff ens2 ens1 = [];;

let print_bool = function
| true -> print_string "true\n"
| false -> print_string "false\n"
;;

let () = print_string "\n"; print_bool (egaux [1;3;4;2] [1;2;3;4]);;

type partie = Finie of int list | Cofinie of int list;;

let est_vide_W = function
| Finie [] -> true
| _ -> false
;;

let compl_W = function
| Finie a -> Cofinie a
| Cofinie a -> Finie a
;;

let rec union_W ens1 ens2 = match ens1,ens2 with
| Cofinie a, Finie b -> Cofinie (diff a b)
| Cofinie a, Cofinie b -> Cofinie (inter a b)
| Finie a, Cofinie b -> union_W (Cofinie b) (Finie a)
| Finie a, Finie b -> Finie (union a b)
;;

let rec inter_W ens1 ens2 = match ens1,ens2 with
| Finie a, Finie b -> Finie (inter a b)
| Cofinie a, Cofinie b -> Cofinie (union a b)
| Finie a, Cofinie b -> Finie (diff a b)
| Cofinie a, Finie b -> inter_W ens2 ens1
;;

let rec disj_W ens1 ens2 = match ens1, ens2 with
| Finie a, Finie b -> if inter a b = [] then true else false
| Cofinie a, Finie b -> if diff b a=[] then true else false
| Finie _, Cofinie _ -> disj_W ens2 ens1
| Cofinie _, Cofinie _ -> true
;;

let rec inclus_W ens1 ens2 = match ens1, ens2 with
| Finie a, Finie b -> inclus a b
| Finie a, Cofinie b -> if inter a b = [] then true else false
| Cofinie a, Finie b -> false
| Cofinie a, Cofinie b -> if inclus b a then true else false
;;

let rec egaux_W ens1 ens2 = match ens1, ens2 with
| Finie _, Cofinie _ | Cofinie _, Finie _ -> false
| Finie a, Finie b | Cofinie a, Cofinie b -> egaux a b
;;

let rec complementaires_W ens1 ens2 = match ens1, ens1 with
| Cofinie _, Cofinie _ | Finie _, Finie _ -> false
| Cofinie a, Finie b -> egaux a b
| Finie _, Cofinie _ -> complementaires_W ens2 ens1
;;

let union_U f1 f2 = f1 || f2;;

let inter_U f1 f2 = f1 && f2;;

let diff_U f1 f2 = f1 && not f2;;

let compl_U f1 = not f1;;

let rec convert ens n = match ens with
| [] -> false
| h::t when h=n -> true
| _::t -> convert t n;; 

let rec convert_W ens n = match ens with
| Finie a -> convert a n
| Cofinie a -> not (convert a n)
;;

(*
let ($) f1 f2 = fun h -> f1 (f2 h);;

let fo42 f1 f2 = function x -> not ((f1 x) && (f2 x))

let not f = fo42 f;;  

let union_UM f1 f2 = fo42 (not $ f1) (not $ f2);;

let inter_UM f1 f2 = not (fo42 f1 f2);; (* On utilise l'involutivité du not *)

let diff_UM f1 f2 = not (fo42 f1 (not $ f2));;

let compl_UM f1 = fo42 (f1) (fun _-> true);; *)


(* Algèbre de Peano *)

type pint = Zero | S of pint;;

let rec int_of_pint = function
| Zero -> 0
| S a -> 1+(int_of_pint a)
;;

let print_pint a = print_int (int_of_pint a);;

let rec pint_of_int = function
| 0 -> Zero
| k when k<0 -> failwith "Invalid int"
| k -> S (pint_of_int (k-1))
;;

let rec add x y = match y with
| Zero -> x
| S a -> add (S x) (a)
;;

let rec mul x y = match y with
| Zero -> Zero
| S a -> add (mul x a) (x)
;;

(* Complexité mul : x*a *)

let is_even a = int_of_pint a mod 2 = 0 ;;

let rec div2 = function
| Zero | S Zero -> Zero
| S S a -> S (div2 a)
;;
A
let rec mul2 = function
| Zero -> Zero
| S a -> S (S (mul2 a))
;;

let rec mul_bis x = function
| Zero -> Zero
| y when is_even y -> mul_bis (mul2 x) (div2 y)
| y -> add (mul_bis (mul2 x) (div2 y)) x
;;

(* Complexité de la fonction mul_bis : *)

let rec fac = function
| AZero -> Zero
| S Zero -> S Zero
| S a -> mul (S a) (fac a)
;;

(*
let rec divMod a b = match b with
   | Zero -> (a,Zero)
   | k    -> let rec modCheck acc = function
		| Zero,count when count=int_of_pint b -> (pint_of_int (acc+1),Zero)
		| Zero,count -> (pint_of_int acc,pint_of_int count)
             	| S s, count when count=int_of_pint b -> modCheck (acc+1) (a,0)
		| S s, count -> modCheck acc (s,count+1)
	     in modCheck 0 (int_of_pint a,0)
;;
*)

let divMod a b = let a_i = int_of_pint a and b_i = int_of_pint b in
		 (a_i/b_i |> pint_of_int,a_i mod b_i |> pint_of_int)
;;

let pgcd a b = 
    let rec aux = function
    | Zero -> Zero
    | S k -> match (divMod a (S k),divMod b (S k)) with
	   | (_,Zero),(_,Zero) -> S k
	   | _ ->  aux k
in aux ((max (a |> int_of_pint) (b |> int_of_pint)) |> pint_of_int);;
