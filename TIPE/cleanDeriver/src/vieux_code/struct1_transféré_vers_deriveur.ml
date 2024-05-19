(*---------Types pour construire des expressions----------*)
type operation_etendues = Mult | Sum | Quotient;;
type constante=bool*int*int;; (* Constante rationnelle et i, (i=(1,1,true)*)
type fonction_etendue = Exp | Sin | Cos | Tan;;
type expression_etendue = | Node of (operation_etendues*expression_etendue*expression_etendue)
                          | Fonction of fonction_etendue*expression_etendue 
                          | Puissance of constante 
                          | Leaf of constante;;

type equation=expression_etendue*expression_etendue;;

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

let exp1=Exp(Puissance ((false,1,1)));;
let exp2=Node(Mult,[Node(Sum,
          [Puissance (false,1,1);Puissance (false,2,1);Puissance (false,4,1)]);
          Exp (Puissance (false,8,1));Node(Sum,[Puissance (false,16,1);
          Puissance (false,32,1);Puissance (false,64,1)]);
          Node(Mult,[Puissance (false,128,1);
          Puissance (false,256,1)])])
let exp0=Node (Sum,[exp2;Node (Mult, [Leaf (false,-1,1);exp2]);Leaf (true,1,1)]);;

let rec gcd a b=
  if a=0 && b=0 then 1 
  else if a<0 || b<0 then gcd (abs a) (abs b)  else
  match a,b with
  | 0,a | a,0 -> a
  | a,b -> gcd (max a b-(max a b/min a b)*min a b) (min a b)

and sg a b=(if a<0 then -1 else 1 )*(if b<0 then -1 else 1 );;
let xor a b = (a || b) && not (a && b)

let mult_rat (bo,a,b) (bo',a',b') =
  let div=gcd (a*a') (b*b') in (* Le if à la ligne suivante permet de traîter le produit complexe : i*i=-1*)
  (xor bo bo',(if (bo && bo') then -1 else 1 )*a*a'/div,b*b'/div);;
let real (a,b,c) = a=false;;
let nil_rat (a,b,c)= b=0;;

let sum_real_rat (c,a,b) (c',a',b')=
  if c || c' then failwith "Complex powers are not allowed except for the exp function" else
  let p=a*b'+a'*b and q=b*b' in
  let div=gcd p q in false,p/div,q/div;;

let sum_compatible_rat (c,a,b) (c',a',b')=
  if c<>c' then failwith "Imaginaires purs et réels ne sont pas sommables" else
  let p=a*b'+a'*b and q=b*b' in
  let div=gcd p q in c,p/div,q/div;;

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


let rec flatten_op (Node (op,l))=
  let rec iter =function
  | [] -> []
  | Node (o,l')::t when o=op -> l'@iter t
  | a::t -> a::iter t
  in iter (l |> List.map flatten)
and flatten=function
| Node (op,l) ->  Node (op,flatten_op (Node (op,l))) 
| Puissance p -> Puissance p;
| Exp exp -> Exp (flatten exp)
| a -> a;;

(* Tri des différents termes d'un noeud produit/somme. Permet de simplifier certains termes*)
let sort_op_node_sum l=
  let rec insert a =function
  | [] -> [a]
  | h::t when a<h ->a::h::t
  | h::t -> h::insert a t in
  let rec insert_rat (bo,a,b)=function
  | [] -> [(bo,a,b)]
  | (bo',a',b')::t when bo=bo' ->  sum_compatible_rat (bo,a,b) (bo',a',b')::t
  | (bo',a',b')::t -> if bo=false then (bo,a,b)::(bo',a',b')::t else (bo',a',b')::insert_rat (bo,a,b) t 
  (* On choisit la convention de mettre les rationnels réels devant les imaginaires purs*) in
  let rec sorter=function
  | []->([],[],[],[])
  | Node (op,exp_l)::t -> let (l1,l2,l3,l4)=sorter t in (insert (Node (op,exp_l)) l1,l2,l3,l4)
  | Exp exp::t -> let (l1,l2,l3,l4)=sorter t in (l1,insert (Exp exp) l2,l3,l4)
  | Leaf r::t -> let (l1,l2,l3,l4)=sorter t in (l1,l2,insert_rat r l3,l4)
  | Puissance p::t -> let (l1,l2,l3,l4)=sorter t in (l1,l2,l3,insert (Puissance p) l4)
in sorter l
;;

let sort_op_node_mult l=
  let rec insert a =function
  | [] -> [a]
  | h::t when a<h ->a::h::t
  | h::t -> h::insert a t in
  let rec insert_rat (bo,a,b)=function
  | [] -> [(bo,a,b)]
  | (bo',a',b')::t -> mult_rat (bo,a,b) (bo',a',b')::t
  (* On choisit la convention de mettre les rationnels réels devant les imaginaires purs*) in
  let rec sorter=function
  | []->([],[],[],(false,0,1))
  | Node (op,exp_l)::t -> let (l1,exp,cst,pow)=sorter t in (insert (Node (op,exp_l)) l1,exp,cst,pow)
  | Exp expr::t -> let (l1,exp,cst,pow)=sorter t in (l1,insert (expr) exp,cst,pow)
  | Leaf r::t -> let (l1,exp,cst,pow)=sorter t in (l1,exp,insert_rat r cst,pow)
  | Puissance p::t -> let (l1,exp,cst,pow)=sorter t in (l1,exp,cst,(sum_real_rat p pow))
in sorter l
;;

(* Permet de simplifier les constantes et, dans le cas d'un produit, les puissances et les exponentielles*)

type status=E of expression | Zero | One;;

let remove_empty exp=
  let rec aux= function
  | Leaf (false,0,1) -> Zero
  | Leaf (false,1,1) | Puissance (false,0,_) -> One
  | Exp (exp) when aux exp=Zero -> One
  | Node (Sum,[]) -> Zero
  | Node (Mult,[]) -> One
  | Node (Sum,l) -> let l'=List.filter (fun v->match aux v with | Zero -> false | v -> true) l in if l'=[] then Zero else E (Node (Sum,l'))
  | Node (Mult,l) -> let l'=List.map (fun v->aux v) l in if not (List.for_all (fun v->v<>Zero) l') then Zero 
                    else let l'=List.filter (fun v->match aux v with | One -> false |  v -> true) l in if l'=[] then One else E (Node (Mult,l'))
  | a -> E a
in match aux exp with
| Zero -> Leaf (false,0,1)
| One -> Leaf (false,1,1)
| E v-> v

(* Simplifie avant-tout les facteurs d'un produit pour les réduire à 3 termes (aux sommes et produits près -> cf. distribuer et flatten)*)
let rec simplify =function
| Exp exp -> Exp (simplify exp)
| Node (Mult,l) -> let (l1,exp,cst,pow)=sort_op_node_mult l in
          let res= ref [] in
          if l1<>[] then
            res:=l1 |> List.map (function | Node (op,[a]) -> a | a -> a) else ();
          if cst<>[] then
            res:=List.map (fun v-> Leaf v) cst@ !res else ();
          if exp<>[] then
              res:=Exp (if List.length exp=1 then List.hd exp else Node (Sum,exp))::!res else ();
          if not (nil_rat pow) then
            res:=Puissance pow::!res else ();
          Node (Mult,!res |> List.map simplify)
| Node (Sum,l) -> let (l1,exp,cst,pow)=sort_op_node_sum l in 
          let res= ref [] in
          if l1<>[] then
            res:=l1 |> List.map (function | Node (op,[a]) -> a | a -> a) else ();
          if cst<>[] then
            res:=List.map (fun v-> Leaf v) cst@ !res else ();
          if exp<>[] then
            res:=exp@ !res else ();
          if pow<>[] then (* Optimisation à faire ici -> cas où pow est une constante*)
            res:=pow@ !res else ();
          Node (Sum,!res |> List.map simplify)
| a -> a ;;

let distribuer l=
  let rec filter =function
  | [] ->[],[]
  | Node(Sum,l')::t -> let l_sum,l_other=filter t in l'::l_sum,l_other
  | h::t -> let l_sum,l_other=filter t in l_sum,h::l_other
  in let sums,other=filter l in
  (* Distribue les sommes*)
  let rec iterator=function
  | [] -> []
  | [a] -> List.map (fun v-> v::other) a
  | l::t' -> let l'=iterator t' in List.fold_left (fun h v->h@List.map (fun l''->v::l'') l') [] l
  in (iterator sums |> List.map (fun l->Node (Mult,l)));;

let rec canoniser exp=
  let exp'=exp |> flatten in
  match exp' with
  | Node (Sum,l) -> (Node (Sum,List.map canoniser l)) |> simplify |> flatten
  | Node (Mult,l) -> begin match distribuer l with
                      | [a] -> canoniser a
                      | l -> Node (Sum,l) |> flatten |> simplify |> flatten
                    end
  | Exp e -> Exp (canoniser e)
  | Puissance a->Puissance a
  | Leaf a-> Leaf a ;;
let clean_derivation exp = exp |> derivate |> flatten |> simplify |> flatten;;

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
| Fonction (Exp,exp) -> Fonction (Exp,fct_convert exp)
| Fonction (Sin,exp) -> let cv=fct_convert exp in Node (
                          Quotient,
                          Node(
                            Sum,
                            Fonction (
                              Exp,
                              Node(Mult,Leaf (true,1,1),cv)
                            ),
                            Node(
                              Mult,Leaf (false,-1,1),
                              Fonction (
                                Exp,
                                Node(Mult,Leaf (true,-1,1),cv))
                            )
                          ),
                          Leaf (false,2,1))
| Fonction (Cos,exp) -> let cv=fct_convert exp in Node (
                        Quotient,
                        Node(
                          Sum,
                          Fonction (
                            Exp,
                            Node(Mult,Leaf (true,1,1),cv)
                          ),
                          Fonction(
                            Exp,
                            Node(Mult,Leaf (true,-1,1),cv)
                          )
                        ),
                        Leaf (false,2,1))
| Fonction(Tan,exp) -> Node (Quotient,fct_convert (Fonction (Sin,exp)),fct_convert (Fonction (Cos,exp)))
| Node(op,exp1,exp2) -> Node(op,fct_convert exp1,fct_convert exp2)
| Puissance a-> Puissance a
| Leaf a -> Leaf a;;

let rec reduce_to_simple_quotient =function (* Ramène un expression à un quotient de la forme A/B, avec A et B sans quotients*)
| Fonction (Exp,exp) -> let exp'=reduce_to_simple_quotient exp in begin match exp' with 
                                                              | Fonction (_,_) -> failwith "Une composition d'exponentielles n'est pas autorisée"
                                                              | Node (Quotient,_,_) -> failwith "Quotient interdit à l'intérieur d'une exponentielle !" 
                                                              | a -> Fonction (Exp,a) end
| Fonction (_,exp) -> failwith "Les fonctions sin, cos et tan sont censées être converties en exp"
| Node (Sum,exp1,exp2)->let exp1',exp2'=reduce_to_simple_quotient exp1,reduce_to_simple_quotient exp2 in
                        begin match exp1',exp2' with
                        | (Node(Quotient,a,b) : expression_etendue),Node(Quotient,c,d) ->Node (Quotient,Node(Sum,Node(Mult,a,d),Node(Mult,b,c)),Node(Mult,b,d))
                        | Node(Quotient,a,b),c | c,Node(Quotient,a,b)-> Node(Quotient,Node(Sum,a,Node(Mult,c,b)),b)
                        | a,b -> Node (Sum,a,b)
                        end
| Node (Mult,exp1,exp2)->let exp1',exp2'=reduce_to_simple_quotient exp1,reduce_to_simple_quotient exp2 in
                        begin match exp1',exp2' with
                        | Node(Quotient,a,b),Node(Quotient,c,d) -> Node(Quotient,Node(Mult,a,c),Node(Mult,b,d))
                        | Node(Quotient,a,b),c | c,Node(Quotient,a,b)->Node(Quotient,Node(Mult,a,c),b)
                        | a,b->Node(Mult,a,b)
                        end
| Node(Quotient,exp1,exp2) ->let exp1',exp2'=reduce_to_simple_quotient exp1,reduce_to_simple_quotient exp2 in
                        begin match exp1',exp2' with
                        | Node(Quotient,a,b),Node(Quotient,c,d) -> Node(Quotient,Node(Mult,a,d),Node(Mult,b,c))
                        | Node(Quotient,a,b),c | c,Node(Quotient,a,b)->Node(Quotient,a,Node(Mult,b,c))
                        | a,b->Node(Quotient,a,b)
end
| a->a;;

let delete_prod ((exp1,exp2) : equation) : equation=
  let exp1',exp2'=reduce_to_simple_quotient exp1,reduce_to_simple_quotient exp2 in
  match exp1',exp2' with
  | Node(Quotient,a,b),Node(Quotient,c,d) -> Node(Mult,a,d),Node(Mult,c,b)
  | Node(Quotient,a,b),c | c,Node(Quotient,a,b)-> Node(Mult,c,b),a
  | a,b-> a,b;;

let rec partial_conversion =function
| Fonction (Exp,exp) -> Exp (partial_conversion exp)
| Node (op,e1,e2) when op=Sum || op=Mult->Node ((if op=Sum then Sum else Mult),[partial_conversion e1;partial_conversion e2])
| Puissance a -> Puissance a
| Leaf a-> Leaf a
| _ -> failwith "Conversion appliquée à une expression incompatible" ;;

let compare exp1 exp2=
  let e1,e2=(exp1 |> fct_convert,exp2 |> fct_convert) |> delete_prod in
  let e1',e2'=partial_conversion e1,partial_conversion e2 in
  canoniser (Node(Sum,[e1';Node(Mult,[Leaf (false,-1,1);e2'])]))
;;