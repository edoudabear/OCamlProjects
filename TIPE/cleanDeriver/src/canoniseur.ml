(* PARTIE II DU CODE : solveur d'équations spécifiques *)
open Deriveur;;
open Convertisseur;;

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


(*let rec exp_composition_depth=function
| Exp e -> 1 + exp_composition_depth e
| Node (_,l) -> List.map exp_composition_depth l |> List.fold_left max 0 
| _-> 0;;*)

















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

(* Pseudo-prototype de forme_canonique [Plus utilisé]*)
  let fc=ref (Fun.id);;


(* Fonction qui s'assure d'avoir l'ensemble des sommes AVANT les produits dans la représentation arborescente *)
(* Traîtement de l'exponentielle défaillant *)

let rec order_nodes t =
  (*Printf.printf "Call 0 :\n";
  print_struct_s t;
  Printf.printf "\n";*)
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
| Leaf (_,0,_) -> Leaf (false,0,1)
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

(* Vérifie si une forme pseudo-canonique (somme de formes canoniques non-simplifiées en termes de coefficients) est équivalente à 0*)
let rec equiv_nul = function
| Node (Sum,l)->List.for_all (fun v->equiv_nul v) l
| Node (Mult,l) -> List.exists (fun v-> equiv_nul v) l
| Puissance _ -> false
| Leaf (_,a,_) -> a=0
| Exp _ -> false;;

let rec forme_canonique e= 
 let e'=e |> order_nodes in 
 (*Printf.printf "\nINPUT : "; 
 print_struct_s e;
 Printf.printf "\nOUTPUT : " ;
 print_struct_s e'; Printf.printf "\n\n";*)
 e' |> handle_sum false |> factorisation 
 
 and
(* Fonction handle_sum : *)
handle_sum b exp=
  (*Printf.printf "Call handle_sum : \n";print_struct_s exp; Printf.printf "\n";*)
  match b,exp with
| _,Node (Mult,l) -> Node (Mult,factorize l)
| false,Node (Sum,l) -> Node(Sum,List.map (handle_sum true) l)
| true,Node (Sum,l) -> failwith "Erreur d'exécution (handle_sum)"
| _,Exp a -> Node (Mult,[Exp (forme_canonique a);Puissance (false,0,1);Leaf (false,1,1)])
| _,Puissance a -> Node(Mult,[Exp (Leaf (false,0,1));Puissance a;Leaf (false,1,1)])
| _,Leaf (_,0,_) -> Leaf (false,0,1)
| _,Leaf a -> Node(Mult,[Exp (Leaf (false,0,1));Puissance (false,0,1);Leaf a])
(* Etape indispensable : simplifie les produits d'exp,leaf et puissance *)
and factorize l=
  let (e,p,c)=ref ([Leaf (false,0,1)]),ref (false,0,1),ref (false,1,1) in
  let rec iter=function
  | [] -> [(if !e= [Leaf (false,0,1)] then Exp(Leaf (false,0,1)) else begin
          let e2=forme_canonique (if List.length !e = 1 then (List.hd !e) else (Node (Sum,!e))) in
            if equiv_nul e2 then Exp(Leaf (false,0,1)) else
              Exp e2
          end);Puissance !p; Leaf !c]
  | Puissance a::t -> p:=sum_compatible_rat a !p;iter t
  | Leaf a::t -> c:=mult_rat a (!c); iter t
  | Exp a::t -> if !e = [Leaf (false,0,1)] then match a with | Leaf (_,0,_) -> e:=[Leaf (false,0,1)] | _ -> e:= [a] else e:=a::(!e); iter t
  | Node(Mult,l)::t ->iter l@t
  | a ->print_l a; failwith "Forme incorrecte"
  in
  iter l;;

let ()=fc := forme_canonique;;



let compare_eq exp1 exp2=
  (*Printf.printf "comparaison :\n";*)
  let e1,e2=(exp1 |> fct_convert,exp2 |> fct_convert) |> delete_prod in
  let e1',e2'=partial_conversion e1,partial_conversion e2 in
  (*Printf.printf "Formes converties \ne1 : ";
  e1' |> to_extended_exp |> print_struct;Printf.printf "\ne2 : "; e2' |> to_extended_exp |> print_struct;
  Printf.printf "\n";*)
  let e=forme_canonique (Node(Sum,[e1';Node(Mult,[Leaf (false,-1,1);e2'])])) in
  match e with
  | Node (Sum,[Leaf (_,0,_)]) -> Printf.printf "true\n"; 1
  | a -> Printf.printf "false\n"; 0
;;