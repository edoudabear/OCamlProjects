open Deriveur;;
(*---------Types pour construire des expressions dérivables----------*)
type operation = Mult | Sum;;
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

let is_3ple_null=function
| (_,0,_) -> true
| _ -> false

(* Conversion expression étendue en expression dérivable : *)
(* Conversion des sin,exp et tan en exp *)
let rec fct_convert=function
| Fonction_e (Exp_e,exp) -> Fonction_e (Exp_e,fct_convert exp)
| Fonction_e (Sin_e,exp) -> let cv=fct_convert exp in Node_e (
                          Mult_e,
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
                          Leaf_e (true,1,2))
| Fonction_e (Cos_e,exp) -> let cv=fct_convert exp in Node_e (
                        Mult_e,
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
                        Leaf_e (false,1,2))
| Fonction_e(Tan_e,exp) -> let exp'=fct_convert exp in Node_e (Quotient_e,  
                                                  Node_e (
                                                    Mult_e,
                                                    Leaf_e(true,-1,1),
                                                    (Node_e (
                                                      Sum_e,
                                                      Fonction_e (
                                                        Exp_e,
                                                        Node_e(Mult_e,Leaf_e (true,1,1),exp')
                                                      ),
                                                      Node_e( 
                                                        Mult_e,
                                                        Leaf_e (false,-1,1),
                                                          Fonction_e(
                                                            Exp_e,
                                                            Node_e(Mult_e,Leaf_e (true,-1,1),exp')
                                                          )
                                                        )
                                                      )
                                                    )
                                                  ),
                                                  Node_e(
                                                    Sum_e,
                                                    Fonction_e (
                                                      Exp_e,
                                                      Node_e(Mult_e,Leaf_e (true,1,1),exp')
                                                    ),
                                                    Fonction_e(
                                                      Exp_e,
                                                      Node_e(Mult_e,Leaf_e (true,-1,1),exp')
                                                    )
                                                  )
                                                )
| Node_e(op,exp1,exp2) -> Node_e(op,fct_convert exp1,fct_convert exp2)
| Puissance_e a-> Puissance_e a
| Leaf_e a -> Leaf_e a;;

let rec reduce_to_simple_quotient =function (* Ramène un expression à un quotient de la forme A/B, avec A et B sans quotients*)
| Fonction_e (Exp_e,exp) -> let exp'=reduce_to_simple_quotient exp in begin match exp' with
                                                              | Fonction_e (a,_) when a <> Exp_e -> failwith "La conversion partielle des fonctions est censée empêcher d'avoir d'autres fonctions que exp"
                                                              | Node_e (Quotient_e,_,_) -> failwith "Quotient interdit à l'intérieur d'une exponentielle !" 
                                                              | a -> Fonction_e (Exp_e,a) end
| Fonction_e (_,exp) -> failwith "Les fonctions sin, cos et tan sont censées être converties en exp"
| Node_e (Sum_e,exp1,exp2)->let exp1',exp2'=reduce_to_simple_quotient exp1,reduce_to_simple_quotient exp2 in
                        begin match exp1',exp2' with
                        | (Node_e(Quotient_e,a,b) : expression_etendue),Node_e(Quotient_e,c,d) -> Node_e (Quotient_e,Node_e(Sum_e,Node_e(Mult_e,a,d),Node_e(Mult_e,b,c)),Node_e(Mult_e,b,d))
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
                        | Node_e(Quotient_e,a,b),c -> Node_e(Quotient_e,a,Node_e(Mult_e,b,c))
                        | c,Node_e(Quotient_e,a,b) -> Node_e(Quotient_e,Node_e(Mult_e,b,c),a)
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

let rec print_exp_latex_s=function
| Node (_,[])      -> Printf.printf "Warning : empty op node";
| Node(Mult,l)     -> print_exp_latex_s (List.hd l); List.iter (fun v->Printf.printf "*";print_exp_latex_s v) (List.tl l)
| Node(Sum,l)      -> Printf.printf "\n";print_exp_latex_s (List.hd l); List.iter (fun v-> Printf.printf "+\n";print_exp_latex_s v) (List.tl l);Printf.printf "\n"
| Exp a            -> Printf.printf "e^{"; print_exp_latex_s a; Printf.printf "}";
| Leaf(bo,p,q)     -> Printf.printf "%f%s" (float_of_int p/.float_of_int q) (if bo then "i" else "");
| Puissance(bo,p,q)-> Printf.printf "x^{%d%s}" (p/q) (if bo then "i" else "");;

let rec print_struct_s = function
| Node (op,l) -> Printf.printf "Node (%s,[\n" (if op=Mult then "Mult" else "Sum") ; List.iter (fun s->print_struct_s s;Printf.printf ";\n") l ;Printf.printf "])"
| Leaf (b,p,q) -> Printf.printf "Leaf (%b,%d,%d)" b p q;
| Puissance (b,p,q) -> Printf.printf "Puissance (%b,%d,%d)" b p q;
| Exp a-> Printf.printf "Exp ("; print_struct_s a; Printf.printf ")";;