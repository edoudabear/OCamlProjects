(*---------Types pour construire des expressions étendues----------*)
type operation_etendues = Mult_e | Sum_e | Quotient_e;;
type constante=bool*int*int;; (* Constante rationnelle et i, (i=(true,1,1)*)
type fonction_etendue = Exp_e | Sin_e | Cos_e | Tan_e;;
type expression_etendue = | Node_e of (operation_etendues*expression_etendue*expression_etendue)
                          | Fonction_e of fonction_etendue*expression_etendue 
                          | Puissance_e of constante 
                          | Leaf_e of constante;;

type equation=expression_etendue*expression_etendue;;


(* AVANT DE COMPILER

   Il y a un makefile permettant de compiler l'analyseur syntaxique et le fichier dériveur d'un seul coup
   Ce fichier met également à jour le ficher mis en ligne et accessible par requête POST du serveur à 
   l'adresse https://e.diskloud.fr/calculus, avec pour paramètre dans le corps 'exp'
*)


let print_f=function
| Exp_e -> Printf.printf "exp"
| Cos_e -> Printf.printf "cos"
| Sin_e -> Printf.printf "sin"
| Tan_e -> Printf.printf "tan";;

(*------------Fonctions manipulant des expressions étendues---------------*)
let rec print_expression = function
| Node_e (op,exp1,exp2) -> begin match op with
                        | Sum_e -> print_expression exp1 ; print_string "+\n" ; print_expression exp2; ()
                        | Mult_e -> Printf.printf "(" ;print_expression exp1 ; Printf.printf ")*(" ; print_expression exp2; Printf.printf ")" ; ()
                        | Quotient_e -> Printf.printf "(" ;print_expression exp1 ; Printf.printf ")/(" ; print_expression exp2; Printf.printf ")" ; ()
                        end
| Fonction_e (f,exp1) -> print_f f;print_string "(";print_expression exp1 ;print_string ")"; ()
| Leaf_e (bo,a,b) -> if bo then Printf.printf "i"; Printf.printf "%d" a ; if b<>1 then Printf.printf "/%d" b
| Puissance_e (bo,a,b) -> Printf.printf "x";Printf.printf (if bo then "^i%d" else "^%d") a;if b<>1 then Printf.printf "/%d" b;;

(* Impression d'une expression générale avec saut de ligne*)
let println_expression expr=expr |> print_expression; Printf.printf "\n";;

(* Impression des noms de fonctions usuelles au format TeX (avec le \)*)
let printf_f_latex = function
| Cos_e -> Printf.printf "\\cos"
| Sin_e -> Printf.printf "\\sin"
| Tan_e -> Printf.printf "\\tan"
| Exp_e -> Printf.printf "\\exp"

(* Impression de la structure (permet de savoir l'ordre choisi dans
   l'arbre sur des opérations commutatives, mais moins lisible que
   la représentation usuelle)*)
let rec print_struct=function
| Node_e (op,exp1,exp2) -> Printf.printf "Node_e(";begin match op with
                        | Sum_e -> Printf.printf "Sum_e,";print_struct exp1 ; Printf.printf ","; print_struct exp2; Printf.printf ")"
                        | Mult_e -> Printf.printf "Mult_e,";print_struct exp1 ; Printf.printf ","; print_struct exp2; Printf.printf ")"
                        | Quotient_e -> Printf.printf "Quotient_e,";print_struct exp1 ; Printf.printf ","; print_struct exp2; Printf.printf ")"
                        end
(*  --Remplacé par exp(ln(x)k) dans la structure--
   | Pow (k,exp) -> print_string "(";print_struct exp;Printf.printf ")^%s " (float_to_good_str k); *)
| Fonction_e (f,exp1) -> Printf.printf "Fonction_e (";print_f f;print_string ",";print_struct exp1 ;print_string ")"; ()
| Leaf_e (bo,a,b) -> Printf.printf "Leaf_e (%b,%d,%d)" bo a b; ()
| Puissance_e (bo,a,b) -> Printf.printf "Puissance_e (%b,%d,%d)" bo a b;;


(* Impression d'une expression générale au format TeX*)
let rec print_exp_latex=function
| Node_e (op,exp1,exp2) -> begin match op with
          | Sum_e -> print_exp_latex exp1 ; print_string "+" ; print_exp_latex exp2; ()
          | Mult_e -> Printf.printf "(" ;print_exp_latex exp1 ; Printf.printf ")\\times(" ; print_exp_latex exp2; Printf.printf ")" ; ()
          | Quotient_e -> Printf.printf "\\frac{" ;print_exp_latex exp1 ; Printf.printf "}{" ; print_exp_latex exp2; Printf.printf "}" ; ()
          end
| Fonction_e (f,exp1) -> printf_f_latex f;print_string "(";print_exp_latex exp1 ;print_string ")"; ()
| Leaf_e (bo,a,b) -> if b=1 then Printf.printf "%d" a else Printf.printf "\\frac{%d}{%d}" a b; if bo then Printf.printf "i"; ()
| Puissance_e (bo,a,b) -> Printf.printf "x^{";Printf.printf (if bo then "i%d" else "%d") a; if b<>1 then Printf.printf "/%d}" b else Printf.printf "}" ;;

(* Nomenclature des dérivées (pour celles usuelles comme les variables muettes)*)
let derivee_f f exp1=match f with
| Sin_e-> Fonction_e (Cos_e,exp1)
| Cos_e -> Node_e (Mult_e,Leaf_e(false,-1,1),Fonction_e (Sin_e,exp1))
| Tan_e -> Node_e (Sum_e,Leaf_e (false,1,1),Node_e(Mult_e,Fonction_e(Tan_e,exp1),Fonction_e(Tan_e,exp1)))
| Exp_e -> Fonction_e (Exp_e,exp1);;

(* Pgcd *)
let rec gcd a b=
  if a=0 && b=0 then 1 
  else if a<0 || b<0 then gcd (abs a) (abs b)  else
  match a,b with
  | 0,a | a,0 -> a
  | a,b -> gcd (max a b-(max a b/min a b)*min a b) (min a b)

(* signe *)
and sg a b=(if a<0 then -1 else 1 )*(if b<0 then -1 else 1 );;

(* Fonction de dérivation générale *)
let deriveur expr =
  let rec deriveur=function
  | Node_e (op,exp1,exp2) -> begin match op with 
                          | Sum_e -> Node_e (op,deriveur exp1,deriveur exp2)
                          | Mult_e -> Node_e (Sum_e,Node_e (Mult_e,exp1,deriveur exp2),Node_e (Mult_e,deriveur exp1,exp2))
                          | Quotient_e -> Node_e (Quotient_e,
                                          Node_e (Sum_e,
                                            Node_e (Mult_e,deriveur exp1,exp2),
                                            Node_e(Mult_e,Leaf_e (false,-1,1),Node_e(Mult_e,exp1,deriveur exp2))
                                          )
                                          ,Node_e(Mult_e,exp2,exp2)
                                        )
                          end
  | Fonction_e (f,exp1) -> Node_e (Mult_e,deriveur exp1,derivee_f f exp1)
  | Leaf_e p-> Leaf_e (false,0,1)
  | Puissance_e (bo,a,b) -> Node_e(Mult_e,Leaf_e (bo,a,b),Puissance_e (bo,a-b/gcd (a-b) b,b/gcd (a-b) b))
  in deriveur expr
;;

(* Ou exclusif *)
let xor a b = (a || b) && not (a && b);;

(* Produit de rationnels *)
let mult_rat_e a b =match a,b with
  | (Leaf_e (bo,a,b)),(Leaf_e (bo',a',b')) ->let div=gcd (a*a') (b*b') in (* Le if à la ligne suivante permet de traîter le produit complexe : i*i=-1*)
  (xor bo bo',(if (bo && bo') then -1 else 1 )*a*a'/div,b*b'/div)
  | _ -> failwith "invalid structure";;

(* Indique si la constante est un imaginaire pur ou un réel *)
let real (a,b,c) = a=false;;

(* Constante nulle ? *)
let nil_rat (a,b,c)= b=0;;

(* Puissance entière sur une expression *)
let pow_int_e a k=
  let rec aux b=function
  | k' when k' > 0 -> aux (Leaf_e (mult_rat_e b a)) (k'-1)
  | _ -> b
in aux (Leaf_e (false,1,1)) k;;

exception SpaceError of string;;

(* Somme de constantes compatibles *)
let sum_rat_e a b=match a,b with
| Leaf_e (bo,p,q),Leaf_e (bo',p',q') -> if bo=bo' then Leaf_e (bo,(p*q'+p'*q)/gcd (p+p') (q*q'),(q*q')/gcd (p*q'+p'*q) (q*q'))
                                        else failwith "incompatible rationals"
| _ -> failwith "a or b is not a rational/complex rational"

(* Produit de constantes *)
let pow_rat_e a=function
| Leaf_e (b,p,q)->  if b || p mod q <> 0 then raise (SpaceError "Puissance non-entière")
  else pow_int_e a (p/q)
| _ -> failwith "invalid structure (expected leaf)";;

(* Inverse d'une constante *)
let inv_e=function
| Leaf_e (b,p,q)-> if b then Leaf_e (b,-q,p) else Leaf_e (b,q,p)
| _ -> failwith "Not a rationnal (inv)";;

(* Création d'une constante imaginaire pure *)
let make_complex_leaf_e (_,p,q)=Leaf_e (true,p,q);;

(* Produit itéré *)
let rec produit a =function
| k when k<0 -> Node_e(Quotient_e,Leaf_e (false,1,1), produit a (-k))
| k when k=0 -> Leaf_e (false,1,1)
| k  -> Node_e(Mult_e,a,produit a (k-1));;


let simplificateur a1 a2=
  let rec reducteur_g=function
  | Fonction_e _ -> raise (SpaceError "terme exponentiel non accepté")
  | Node_e (Sum_e,a,b) -> sum_rat_e (reducteur_g a) (reducteur_g b)
  | Node_e (Mult_e,a,b) -> Leaf_e (mult_rat_e (reducteur_g a) (reducteur_g b))
  | Node_e (Quotient_e,a,b) -> Leaf_e (mult_rat_e (reducteur_g b |> inv_e) (reducteur_g a))
  | Leaf_e a -> Leaf_e a
  | Puissance_e _ -> failwith "simplification de puissance non-supportée"
in  match reducteur_g a2 with
    | Leaf_e (false,p,q) when p mod q = 0 -> produit a1 (p/q)
    | _ -> failwith "Puissance non autorisée" 
;;

let simplificateurX a2=
let rec reducteur_g=function
| Fonction_e _ -> raise (SpaceError "terme exponentiel non accepté")
| Node_e (Sum_e,a,b) -> sum_rat_e (reducteur_g a) (reducteur_g b)
| Node_e (Mult_e,a,b) -> Leaf_e (mult_rat_e (reducteur_g a) (reducteur_g b))
| Node_e (Quotient_e,a,b) -> Leaf_e (mult_rat_e (reducteur_g b |> inv_e) (reducteur_g a))
| Leaf_e a -> Leaf_e a
| Puissance_e _ -> failwith "simplification de puissance non-supportée"
in  match (reducteur_g a2) with
| Leaf_e (false,p,q) when p mod q = 0 -> Puissance_e (false,p/q,1)
| _ -> failwith "Puissance non autorisée"
;;