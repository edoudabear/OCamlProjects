(*---------Types pour construire des expressions étendues----------*)
type operation_etendues = Mult_e | Sum_e | Quotient_e;;
type constante=bool*int*int;; (* Constante rationnelle et i, (i=(true,1,1)*)
type fonction_etendue = Exp_e | Sin_e | Cos_e | Tan_e;;
type expression_etendue = | Node_e of (operation_etendues*expression_etendue*expression_etendue)
                          | Fonction_e of fonction_etendue*expression_etendue 
                          | Puissance_e of constante 
                          | Leaf_e of constante;;

type equation=expression_etendue*expression_etendue;;


(* ROADMAP

   -Traîter la factorisation d'expressions similaires
*)
(* AVANT DE COMPILER

   Il y a un makefile permettant de compiler l'analyseur syntaxique et le fichier dériveur d'un seul coup
   Ce fichier met également à jour le ficher mis en ligne et accessible par requête POST du serveur à 
   l'adresse https://e.diskloud.fr/calculus, avec pour paramètre dans le corps 'exp'
*)

(*--------Des exemples d'expressions valides---------------*)
(*
let expr=Node (Sum_e,
  Fonction (Cos,Node(Mult_e,Leaf (false,31,5),Puissance (false,1,1))),
  Node(Sum_e,Leaf(false,5,2),Puissance (false,1,1))
  );;
*)


let print_f=function
| Exp_e -> Printf.printf "exp"
| Cos_e -> Printf.printf "cos"
| Sin_e -> Printf.printf "sin"
| Tan_e -> Printf.printf "tan";;

(*------------Fonctions manipulant des expressions plus générales---------------*)
let rec print_expression = function
| Node_e (op,exp1,exp2) -> begin match op with
                        | Sum_e -> print_expression exp1 ; print_string "+\n" ; print_expression exp2; ()
                        | Mult_e -> Printf.printf "(" ;print_expression exp1 ; Printf.printf ")*(" ; print_expression exp2; Printf.printf ")" ; ()
                        | Quotient_e -> Printf.printf "(" ;print_expression exp1 ; Printf.printf ")/(" ; print_expression exp2; Printf.printf ")" ; ()
                        end
(* --Pow a été enlevé de la structure au profit de exp(ln(x)k)--
   | Pow (k,exp) -> print_string "(";print_expression exp;Printf.printf ")^%s " (float_to_good_str k); *)
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
| Exp_e -> Fonction_e (Exp_e,exp1)

(* Fonction de dérivation générale *)
let deriveur expr =
  let rec deriveur=function
  | Node_e (op,exp1,exp2) -> begin match op with 
                          | Sum_e -> Node_e (op,deriveur exp1,deriveur exp2)
                          | Mult_e -> Node_e (Sum_e,Node_e (Mult_e,exp1,deriveur exp2),Node_e (Mult_e,deriveur exp1,exp2))
                          | Quotient_e -> Node_e (Quotient_e,
                                          Node_e (Sum_e,
                                            Node_e (Mult_e,deriveur exp1,exp2),
                                            Node_e(Mult_e,Leaf_e (false,-1,1),Node_e(Mult_e,exp2,deriveur exp1))
                                          )
                                          ,Node_e(Mult_e,exp2,exp2)
                                        )
                          end
  | Fonction_e (f,exp1) -> Node_e (Mult_e,deriveur exp1,derivee_f f exp1)
  | Leaf_e p-> Leaf_e (false,0,1)
  | Puissance_e (bo,a,b) -> Node_e(Mult_e,Leaf_e (bo,a,b),Puissance_e (bo,a-b,b)) (* Ajouter la simplification par pgcd*)
  in deriveur expr
;;

let rec gcd a b=
  if a=0 && b=0 then 1 
  else if a<0 || b<0 then gcd (abs a) (abs b)  else
  match a,b with
  | 0,a | a,0 -> a
  | a,b -> gcd (max a b-(max a b/min a b)*min a b) (min a b)

and sg a b=(if a<0 then -1 else 1 )*(if b<0 then -1 else 1 );;

let xor a b = (a || b) && not (a && b);;

let mult_rat_e a b =match a,b with
  | (Leaf_e (bo,a,b)),(Leaf_e (bo',a',b')) ->let div=gcd (a*a') (b*b') in (* Le if à la ligne suivante permet de traîter le produit complexe : i*i=-1*)
  (xor bo bo',(if (bo && bo') then -1 else 1 )*a*a'/div,b*b'/div)
  | _ -> failwith "invalid structure";;

let real (a,b,c) = a=false;;
let nil_rat (a,b,c)= b=0;;

let pow_int_e a k=
  let rec aux b=function
  | k' when k' > 0 -> aux (Leaf_e (mult_rat_e b a)) (k'-1)
  | _ -> b
in aux (Leaf_e (false,1,1)) k;;

exception SpaceError of string;;

let sum_rat_e a b=match a,b with
| Leaf_e (bo,p,q),Leaf_e (bo',p',q') -> if bo=bo' then Leaf_e (bo,(p*q'+p'*q)/gcd (p+p') (q*q'),(q*q')/gcd (p*q'+p'*q) (q*q'))
                                        else failwith "incompatible rationals"
| _ -> failwith "a or b is not a rational/complex rational"

let pow_rat_e a=function
| Leaf_e (b,p,q)->  if b || p mod q <> 0 then raise (SpaceError "Puissance non-entière")
  else pow_int_e a (p/q)
| _ -> failwith "invalid structure (expected leaf)";;

let inv_e=function
| Leaf_e (b,p,q)-> if b then Leaf_e (b,-q,p) else Leaf_e (b,q,p)
| _ -> failwith "Not a rationnal (inv)";;

let make_complex_leaf_e (_,p,q)=Leaf_e (true,p,q);;

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


(*let (+$) (x,y) (x',y')=(x+.x',y+.y');;
let ( *$ ) (x,y) (x',y')=(x*.x'-.y*.y',x*.y'+.x'*.y);;
let (/$) (x,y) (x',y')=((x'**2.+.y'**2.),y+.y'/.(x'**2.+.y'**2.))*$(x,y)*$(x',-.y');;


(* Evaluation d'une expression quelconque f en un dyadique x *)
let rec eval f x= match f with
| Node (Sum_e,exp1,exp2)  -> eval exp1 x +$ eval exp2 x
| Node (Mult_e,exp1,exp2) -> eval exp1 x *$ eval exp2 x
| Node (Quotient_e,exp1,exp2) -> eval exp1 x *$ eval exp2 x
| Fonction (f,exp1) -> eval_f f exp1 x
| Leaf (bo,a,b)-> if bo then (0.,(a|>float_of_int)/.(b|>float_of_int)) else ((a|>float_of_int)/. (b|>float_of_int),0.)
| Puissance (bo,a,b) -> (x**(a |> float_of_int),0.)*$(cos ((b|>float_of_int)*.log x),sin ((b|>float_of_int)*.log x))
and eval_f f expr x= match f with
| Exp -> (exp (fst (eval expr x)),0.)*$(cos (snd (eval expr x)),sin (snd (eval expr x)))
| Sin -> (sin (fst (eval expr x)),0.)
| Cos -> (cos (fst (eval expr x)),0.)
| Tan -> (tan (fst (eval expr x)),0.)*)






















































































































(*
(* PARTIE II DU CODE : solveur d'équations spécifiques *)

(*---------Types pour construire des expressions dérivables----------*)
type operation = Mult | Sum;;
type fonction = Exp;;
type expression = Node of (operation*expression list) | Exp of expression | Leaf of constante | Puissance of constante;;

(* Objectifs :
   - Implémenter une dérivation par liste de termes
   - Faire le pont entre la structure d'arbre binaire et la structure d'arbre d'arité quelconque
   Structure utilisée :
   - P1exp(S1)+P2exp(S2)+...+Pnexp(Sn)
   - Développer une fonction capable de distribuer les produits sur les sommes (détecte les (Mult,[Sum;...;Sum]))
   - Se demander s'il n'y a pas plus simple


   Etapes de vérification :
   Entrée : f_user
   1. On dérive l'expression de base f, qu'on note f'                                     OK
   2. On pose l'équation f'=f_user                                                        OK
   3. On supprime les quotients de l'équation en multipliant comme il se doit             OK    
   4. On transfère de la structure expression_étendue à expression, qui est comparable    OK
   5. On compare                                                                          OK
*)

let mult_rat (bo,a,b) (bo',a',b') =
  let div=gcd (a*a') (b*b') in (* Le if à la ligne suivante permet de traîter le produit complexe : i*i=-1*)
  (xor bo bo',(if (bo && bo') then -1 else 1 )*a*a'/div,b*b'/div);;
let real (a,b,c) = a=false;;
let nil_rat (a,b,c)= b=0;;

let is_real (bo,_,_)=bo;;

let sum_real_rat (c,a,b) (c',a',b')=
  if c || c' then failwith "Complex powers are not allowed except for the exp function" else
  let p=a*b'+a'*b and q=b*b' in
  let div=gcd p q in false,p/div,q/div;;

let sum_compatible_rat (c,a,b) (c',a',b')=
  if c<>c' then failwith "Imaginaires purs et réels ne sont pas sommables" else
  let p=a*b'+a'*b and q=b*b' in
  let div=gcd p q in c,p/div,q/div;;

(*exception Incompatible_Pow;;
let rec pow (b,r,i) (b2,r2,i2)=
  if b2 || i2 <> 1 || r2=0 then raise Incompatible_Pow else
  match i2 with
  | 0 -> (false,1,1)
  | 1 -> (b,r,i)
  | k when k<0 -> pow (b,(if b then -i else i),r) (b2,-r2,i2)
  | k when k>0 -> List.fold_left (fun h _  ->mult_rat (b,r,i) h) (b,r,i) (List.init k Fun.id)
  | _ -> failwith "ne se produit jamais"
  ;;*)

let rec update l i exp=Node (Mult,List.mapi (fun j e->if j=i then derivate e else e) l) (* A checker*)
and derivate=function
| Node (Sum,l)-> Node(Sum,List.map derivate l)
| Node (Mult,l) -> Node(Sum,List.mapi (update l) l)
| Exp exp -> Node(Mult,[derivate exp;Exp exp])
| Leaf a -> Leaf (false,0,1)
| Puissance p -> if not (real p) then failwith "Complex powers are not allowed except on exp function" 
                    else if nil_rat p then  Leaf (false,0,1) else Node(Mult,[Leaf p;Puissance (sum_real_rat p (false,-1,1))])
;;


let print_rat (bo,a,b)=if b=1 then Printf.printf "%d" a else Printf.printf "%d/%d" a b;
    if bo then Printf.printf "i";;

let rec print_op_l op=function
| [] -> ()
| h::h2::t -> if op=Mult then Printf.printf "(";display h;if op=Mult then Printf.printf ")" else Printf.printf "\n";Printf.printf "%c" (if op=Sum then '+' else if op=Mult then '*' else '?');print_op_l op (h2::t)
| [h] -> if op=Mult then Printf.printf "(";display h; if op=Mult then Printf.printf ")" else Printf.printf "\n"
and display =function
| Node (op,l) -> print_op_l op l
| Exp exp -> Printf.printf "%s(" "exp";display exp;Printf.printf ")"
| Leaf a -> print_rat a
| Puissance b -> Printf.printf "x";Printf.printf "^";print_rat b
;;


let rec exp_composition_depth=function
| Exp e -> 1 + exp_composition_depth e
| Node (_,l) -> List.map exp_composition_depth l |> List.fold_left max 0 
| _-> 0;;

















let exp1=Exp(Puissance ((false,1,1)));;
let exp2=Node(Mult,[Node(Sum,
          [Puissance (false,1,1);Puissance (false,2,1);Puissance (false,4,1)]);
          Exp (Puissance (false,8,1));Node(Sum,[Puissance (false,16,1);
          Puissance (false,32,1);Puissance (false,64,1)]);
          Node(Mult,[Puissance (false,128,1);
          Puissance (false,256,1)])]);;
let exp2_1=Node(Mult,[Node(Sum,
          [Puissance (false,1,1);
          Exp (Puissance (false,8,1))]);
          Node(Mult,[Puissance (false,128,1);
          Puissance (false,256,1)])])
let exp0=Node (Sum,[exp2;Node (Mult, [Leaf (false,-1,1);exp2])]);;
let exp0_1=Node (Sum,[exp2_1;Node (Mult, [Leaf (false,-5,1);exp2_1]);Leaf (true,1,1)]);;

(* Conversion expression 'restreinte' en expression étendue *)
let rec to_extended_exp=function
| Node (Sum,[]) -> Leaf_e (false,0,1)
| Node (Mult,[]) -> Printf.printf "Attention : expression potentiellement corrompue"; Leaf_e (false,1,1)
| Node (Sum,[a])-> to_extended_exp a
| Node (Mult,[a])-> to_extended_exp a
| Node (Sum,h::t) -> Node_e (Sum_e,to_extended_exp h,to_extended_exp (Node (Sum,t)))
| Node (Mult,h::t) -> Node_e (Mult_e,to_extended_exp h,to_extended_exp (Node (Mult,t)))
| Exp a-> Fonction_e (Exp_e,to_extended_exp a)
| Puissance a -> Puissance_e a
| Leaf a -> Leaf_e a;;

let print_l l=List.iter (fun v->print_expression (to_extended_exp v);Printf.printf ";") l;Printf.printf "\n";;


(* Développe les produits des expressions d'un noeud Node(Mult,..) *)
let distribuer l=
  (* D'abord, on récupère tous les noeuds qui sont des sommes *)
  let rec filter =function
  | [] ->[],[]
  | Node(Sum,l')::t -> let l_sum,l_other=filter t in l'::l_sum,l_other
  | h::t -> let l_sum,l_other=filter t in l_sum,h::l_other
  in let sums,other=filter l in
  (* Distribue les sommes*)
  let rec iterator=function
  | [] -> [other]
  | [a] -> List.map (fun v-> v::other) a
  | l::t -> let f_dev=iterator t in List.fold_left (fun h fact->h@List.map (fun prod->fact::prod) f_dev) [] l
  in (iterator sums |> List.map (fun l->Node (Mult,l)));;


(* Fonction qui s'assure d'avoir l'ensemble des sommes AVANT les produits dans la représentation arborescente *)

let rec order_nodes t = 
  let rec handle_node op l=function
    | Node (op',l') when op=op' -> (List.fold_left (handle_node op) [] l')@l (* On applatit les noeuds op(=Sum | Mult) imbriqués *)
    | a ->a::l
  in match t with
  | Node (Sum,l) -> Node(Sum,List.fold_left (handle_node Sum) [] (List.map order_nodes l))
  | Node (Mult,l) -> let l=(List.map order_nodes l)
                      |> List.fold_left (handle_node Mult) []
                      |> List.map order_nodes
                      |> distribuer
                      |> List.map (function
                      | Node(Mult,l) ->Node(Mult,List.fold_left (handle_node Mult) [] l |> List.sort compare)
                      | _ -> failwith "Ne doit pas se produire")
                      in
    begin match List.length l with 
    | 0 -> Printf.printf "Attention, peut être que la fonction order nodes, gère mal le cas donné (cf. code)\n"; Node (Sum,[])
    | 1 -> List.hd  l
    | _ -> Node (Sum,l)
     end
  | Exp a -> Exp (order_nodes a)
  | a -> a (* Si on a une feuille, rien à faire *)
  ;;

(* Opération appliquée dans canonize : factorise les monomes identiques aux facteurs près*)
let sum_factorize l=
  let l'=List.sort compare l in
  let rec iter acc=function
  | [] -> [acc]
  | Node (Mult,[Exp a;Puissance b;Leaf c])::t -> begin
    match acc with                                              (* On ne somme pas des réels avec des imaginaires *)
    | Node (Mult,[Exp a';Puissance b';Leaf c']) when a'=a && b=b' && is_real c=is_real c'-> iter (Node (Mult,[Exp a;Puissance b;Leaf (sum_compatible_rat c c')])) t
    | _ -> acc::iter (Node (Mult,[Exp a;Puissance b;Leaf c])) t
    end
  | Leaf (_,0,_)::t -> iter acc t
  | a::_ -> print_struct (to_extended_exp a); failwith "mauvaise factorisation"
  in if l'=[] then [Leaf (false,0,1)] else 
  match iter (List.hd l') (List.tl l') 
  |> List.filter (function 
  | Node (Mult,[Exp a;Puissance b;Leaf c]) -> nil_rat c |> not  (* On supprime les monome ayant un coefficient nul*)
  | Leaf (_,0,_) -> false
  | a ->print_struct (to_extended_exp a); failwith "Structure corrompue" 
  ) with
  | [] -> [Leaf (false,0,1)]
  | l -> l;;

let factorisation=function
| Node (Sum,l) -> Node(Sum,sum_factorize l)
| a -> Node (Sum,[a]);;


(* Supprime les noeuds Mult imbriqués, n'a pas l'air de marcher.. au contraire, empêche les factorisations !*)
let rec unbrick_op=function
| Node (Sum,l) -> let l'=List.map unbrick_op l in Node(Sum,List.fold_left (
                fun n_l v-> match v with
                | Node (Sum,l'')->l''@n_l
                | a -> a::n_l ) [] l')
| Node (Mult,l) ->let l'=List.map unbrick_op l in Node(Mult,List.fold_left (
          fun n_l v-> match v with
          | Node (Mult,l'')->l''@n_l
          | a -> a::l ) [] l')
| Exp a-> Exp (unbrick_op a)
| a -> a;;


let rec forme_canonique e= e |> order_nodes |> handle_sum false |> factorisation  and
handle_sum b exp=match b,exp with
| _,Node (Mult,l) -> Node (Mult,factorize l)
| false,Node (Sum,l) -> Node(Sum,List.map (handle_sum true) l)
| true,Node (Sum,l) -> failwith "Erreur d'exécution"
| _,Exp a -> Node (Mult,[Exp (forme_canonique a);Puissance (false,0,1);Leaf (false,1,1)])
| _,Puissance a -> Node(Mult,[Exp (Leaf (false,0,1));Puissance a;Leaf (false,1,1)])
| _,Leaf (_,0,_) -> Leaf (false,0,1)
| _,Leaf a -> Node(Mult,[Exp (Leaf (false,0,1));Puissance (false,0,1);Leaf a])
(* Etape indispensable : simplifie les produits d'exp,leaf et puissance*)
and factorize l=
let (e,p,c)=ref ([Leaf (false,0,1)]),ref (false,0,1),ref (false,1,1) in
let rec iter=function
| [] -> [Exp (forme_canonique (if List.length !e = 1 then List.hd !e else Node (Sum,!e)) |> order_nodes) ; Puissance !p; Leaf !c]
| Puissance a::t -> p:=sum_compatible_rat a !p;iter t
| Leaf a::t -> c:=mult_rat a (!c); iter t
| Exp a::t -> if !e = [Leaf (false,0,1)] then match a with | Leaf (_,0,_) -> e:=[Exp (Leaf (false,0,1)); Puissance (false,0,1);Leaf (false,0,1)] | _ -> e:= [a] else e:=a::(!e); iter t
| Node(Mult,l)::t ->iter l@t
| a ->print_l a; failwith "Forme incorrecte "
in
iter l;;

let rec cleanup =function
| Node (Mult,l) -> let l'=List.map cleanup l in
        if List.for_all (fun v->match v with | Leaf a->not (nil_rat a) | _->true) l' then
          let l'=List.filter (fun a-> a<>(Leaf (false,1,1)) && match a with | Puissance a-> not (nil_rat a) | _->true) l' in
          begin match l' with 
          | [] -> Leaf (false,1,1)
          | [a] -> a
          | _ -> Node (Mult,l')  end

        else Leaf (false,0,1)
| Node (Sum,l) -> let l'=List.map cleanup l in 
        let l'=List.filter (fun v->match v with | Leaf a ->not (nil_rat a) | _ -> true) l' in
        begin match l' with
        | [] -> Leaf (false,0,0)
        | [a] ->a
        | _ -> Node (Sum,l') 
        end
| a->a ;;

























































































(* Conversion expression étendue en expression dérivable : *)
(* Conversion des sin,exp et tan en exp *)
let rec fct_convert=function
| Fonction_e (Exp_e,exp) -> Fonction_e (Exp_e,fct_convert exp)
| Fonction_e (Sin_e,exp) -> let cv=fct_convert exp in Node_e (
                          Quotient_e,
                          Node_e(
                            Sum_e,
                            Fonction_e (
                              Exp_e,
                              Node_e(Mult_e,Leaf_e (true,1,1),cv)
                            ),
                            Node_e(
                              Mult_e,Leaf_e (false,-1,1),
                              Fonction_e (
                                Exp_e,
                                Node_e(Mult_e,Leaf_e (true,-1,1),cv))
                            )
                          ),
                          Leaf_e (false,2,1))
| Fonction_e (Cos_e,exp) -> let cv=fct_convert exp in Node_e (
                        Quotient_e,
                        Node_e(
                          Sum_e,
                          Fonction_e (
                            Exp_e,
                            Node_e(Mult_e,Leaf_e (true,1,1),cv)
                          ),
                          Fonction_e(
                            Exp_e,
                            Node_e(Mult_e,Leaf_e (true,-1,1),cv)
                          )
                        ),
                        Leaf_e (false,2,1))
| Fonction_e(Tan_e,exp) -> Node_e (Quotient_e,fct_convert (Fonction_e (Sin_e,exp)),fct_convert (Fonction_e (Cos_e,exp)))
| Node_e(op,exp1,exp2) -> Node_e(op,fct_convert exp1,fct_convert exp2)
| Puissance_e a-> Puissance_e a
| Leaf_e a -> Leaf_e a;;

let rec reduce_to_simple_quotient =function (* Ramène un expression à un quotient de la forme A/B, avec A et B sans quotients*)
| Fonction_e (Exp_e,exp) -> let exp'=reduce_to_simple_quotient exp in begin match exp' with 
                                                              | Fonction_e (_,_) -> failwith "Une composition d'exponentielles n'est pas autorisée"
                                                              | Node_e (Quotient_e,_,_) -> failwith "Quotient interdit à l'intérieur d'une exponentielle !" 
                                                              | a -> Fonction_e (Exp_e,a) end
| Fonction_e (_,exp) -> failwith "Les fonctions sin, cos et tan sont censées être converties en exp"
| Node_e (Sum_e,exp1,exp2)->let exp1',exp2'=reduce_to_simple_quotient exp1,reduce_to_simple_quotient exp2 in
                        begin match exp1',exp2' with
                        | (Node_e(Quotient_e,a,b) : expression_etendue),Node_e(Quotient_e,c,d) ->Node_e (Quotient_e,Node_e(Sum_e,Node_e(Mult_e,a,d),Node_e(Mult_e,b,c)),Node_e(Mult_e,b,d))
                        | Node_e(Quotient_e,a,b),c | c,Node_e(Quotient_e,a,b)-> Node_e(Quotient_e,Node_e(Sum_e,a,Node_e(Mult_e,c,b)),b)
                        | a,b -> Node_e (Sum_e,a,b)
                        end
| Node_e (Mult_e,exp1,exp2)->let exp1',exp2'=reduce_to_simple_quotient exp1,reduce_to_simple_quotient exp2 in
                        begin match exp1',exp2' with
                        | Node_e(Quotient_e,a,b),Node_e(Quotient_e,c,d) -> Node_e(Quotient_e,Node_e(Mult_e,a,c),Node_e(Mult_e,b,d))
                        | Node_e(Quotient_e,a,b),c | c,Node_e(Quotient_e,a,b)->Node_e(Quotient_e,Node_e(Mult_e,a,c),b)
                        | a,b->Node_e(Mult_e,a,b)
                        end
| Node_e(Quotient_e,exp1,exp2) ->let exp1',exp2'=reduce_to_simple_quotient exp1,reduce_to_simple_quotient exp2 in
                        begin match exp1',exp2' with
                        | Node_e(Quotient_e,a,b),Node_e(Quotient_e,c,d) -> Node_e(Quotient_e,Node_e(Mult_e,a,d),Node_e(Mult_e,b,c))
                        | Node_e(Quotient_e,a,b),c | c,Node_e(Quotient_e,a,b)->Node_e(Quotient_e,a,Node_e(Mult_e,b,c))
                        | a,b->Node_e(Quotient_e,a,b)
end
| a->a;;

let delete_prod ((exp1,exp2) : equation) : equation=
  let exp1',exp2'=reduce_to_simple_quotient exp1,reduce_to_simple_quotient exp2 in
  match exp1',exp2' with
  | Node_e(Quotient_e,a,b),Node_e(Quotient_e,c,d) -> Node_e(Mult_e,a,d),Node_e(Mult_e,c,b)
  | Node_e(Quotient_e,a,b),c | c,Node_e(Quotient_e,a,b)-> Node_e(Mult_e,c,b),a
  | a,b-> a,b;;

let rec partial_conversion =function
| Fonction_e (Exp_e,exp) -> Exp (partial_conversion exp)
| Node_e (op,e1,e2) when op=Sum_e || op=Mult_e->Node ((if op=Sum_e then Sum else Mult),[partial_conversion e1;partial_conversion e2])
| Puissance_e a -> Puissance a
| Leaf_e a-> Leaf a
| _ -> failwith "Conversion appliquée à une expression incompatible" ;;

let compare_eq exp1 exp2=
  Printf.printf "comparaison :\n";
  let e1,e2=(exp1 |> fct_convert,exp2 |> fct_convert) |> delete_prod in
  let e1',e2'=partial_conversion e1,partial_conversion e2 in
  Printf.printf "Formes converties \ne1 : ";
  e1' |> to_extended_exp |> print_struct;Printf.printf "\ne2 : "; e2' |> to_extended_exp |> print_struct;
  Printf.printf "\n";
  let e=forme_canonique (Node(Sum,[e1';Node(Mult,[Leaf (false,-1,1);e2'])])) in
  e |> to_extended_exp |> print_expression;
  match e with
  | Node (Sum,[Leaf (_,0,_)]) -> Printf.printf "true\n";
  | a -> Printf.printf "false\n";
;;
*)