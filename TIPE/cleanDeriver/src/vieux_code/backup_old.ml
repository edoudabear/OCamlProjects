(*---------Types pour construire des expressions----------*)
type monome = (float*int);;
type pol = monome list;;
type matrix=float array array;;

(* Conversion des entiers flottants en chaîne de caractères débarassée des zéros inutiles ajoutés à la conversion*)
let float_to_good_str f=
  let str=string_of_float f in if str.[String.length str-1]='.' then String.sub str 0 (String.length str-1) else 
    begin let str=ref str in
    while (!str).[String.length (!str)-1]='0' do
      str:=String.sub (!str) 0 (String.length (!str)-1)
    done;
    !str end;;


(*-------Opérations élémentaires sur les polynômes---------*)

(* Evaluation en un dyadique x *)
let eval_P p x= List.fold_left (fun j (a_k,p)->j+.a_k*.(x**(p |> float_of_int))) (0.) p;;

(* Dérivée formelle de P *)
let rec derivee_P=function
| [] -> []
| (a,b)::t when b=0 -> derivee_P t
| (a,b)::t -> let bf=float_of_int b in (a*.bf,b-1)::derivee_P t;;

(* Somme de polynômes p1 et p2 (structure préservée)*)
let rec sum_P p1 p2=match p1,p2 with
| p,[] -> p
| (x1,p1)::t1,(x2,p2)::t2 when p1=p2 -> (x1+.x2,p1)::sum_P t1 t2
| (x1,p1)::t1,(x2,p2)::t2 when p1<p2 -> (x1,p1)::sum_P t1 ((x2,p2)::t2)
| p1,p2 -> sum_P p2 p1;;

(* Produit d'un polynôme avec un monôme (structure préservée)*)
let rec mult_P_M po ((k,p) : monome)=match po with
| [] -> []
| (a,b)::t -> (k*.a,b+p)::mult_P_M t (k,p);;

(* Produit de polynômes p1 et p2 (structure préservée)*)
let rec mult_P p1 p2=match p1 with
| [] -> ([] : pol)
| mon::t -> sum_P (mult_P_M p2 mon) (mult_P t p2);;


(* Impression d'un polynôme P dans la console *)
let rec print_P = function
| [] -> ();
| [(a,0)] ->  Printf.printf "%s" (float_to_good_str a);
| [(1.,1)] -> Printf.printf "x";
| [(a,1)] ->  Printf.printf "%sx" (float_to_good_str a);
| [(a,b)] ->  Printf.printf "%sx^%d" (float_to_good_str a) b;
| (a,0)::t -> Printf.printf "%s+" (float_to_good_str a);print_P t;
| (1.,1)::t -> Printf.printf "x"; print_P t;
| (a,1)::t ->  Printf.printf "%sx+" (float_to_good_str a); print_P t;
| (a,b)::t -> Printf.printf "%sx^%d+" (float_to_good_str a) b;print_P t;;

(* Impression d'un polynôme P dans la console au format TeX*)
let rec print_P_latex = function
| [] -> ();
| [(a,0)] ->  Printf.printf "%s" (float_to_good_str a);
| [(1.,1)] -> Printf.printf "x";
| [(a,1)] ->  Printf.printf "%s" (float_to_good_str a);
| [(a,b)] ->  Printf.printf "%sx^{%d}" (float_to_good_str a) b;
| (a,0)::t -> Printf.printf "%s+" (float_to_good_str a);print_P_latex t;
| (1.,1)::t -> Printf.printf "x"; print_P_latex t;
| (a,1)::t ->  Printf.printf "%sx+" (float_to_good_str a); print_P_latex t;
| (a,b)::t -> Printf.printf "%sx^{%d}+" (float_to_good_str a) b;print_P_latex t;; 

(* Simple fonction vérifiant si un polynôme est nul *)
let rec isEmpty = function
| [] -> true
| (0.,_)::t-> isEmpty t
| _ -> false;;

(* fonction qui à un polynôme P associe -P*)
let rec minus_P p=List.map (fun (a,b) -> (-.a,b)) p;;

(*--------Partie peu utile pour le TIPE (a priori) : méthodes de calcul numérique d'intégrales*)
(*

let generate a b dx=
    if a>b then failwith "Wrong order" else
    let rec generate_aux acc=function
    | x when x-.dx<=a-> a::acc
    | x -> generate_aux ((x-.dx)::acc) (x-.dx)
    in generate_aux [b] b;; 

let rec integr_up expr a b dx=
    if a>b then -.(integr_up expr b a dx) else
    let x_i=generate a b dx in
    let rec iter acc=function
    | [] | [_] -> acc
    | x_1::x_2::t -> iter ((eval expr x_2)*.(x_2-.x_1)+.acc) (x_2::t)
    in iter 0. x_i;;

let integr_dn expr a b dx=
if a>b then -.(integr_up expr b a dx) else
  let x_i=generate a b dx in
  let rec iter acc=function
  | [] | [_] -> acc
  | x_1::x_2::t -> iter ((eval expr x_1)*.(x_2-.x_1)+.acc) (x_2::t)
  in iter 0. x_i;;

let integr_rect expr a b dx=let u=integr_up expr a b dx and d=integr_dn expr a b dx in ((d+.u)/.2.,(u-.d)/.2.);;

let integr_simpson expr a b dx=
if a>b then -.(integr_up expr b a dx) else
  let x_i=generate a b dx in
  let rec iter acc=function
  | [] | [_] -> acc
  | x_1::x_2::t -> iter (((x_2-.x_1)/.6.)*.((eval expr x_1)+.4.*.(eval expr ((x_1+.x_2)/.2.))+.(eval expr x_2))+.acc) (x_2::t)
  in iter 0. x_i;;
*)