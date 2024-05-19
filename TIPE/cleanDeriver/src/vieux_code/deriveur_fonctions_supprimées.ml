(* Applatit les opérateurs *)
let rec flatten_op op l=
  let rec iter =function
  | [] -> []
  | Node (o,l')::t when o=op -> l'@iter t
  | a::t -> a::iter t
 in iter (l |> List.map flatten)
and flatten=function
| Node (op,l) ->  Node (op,flatten_op op l) 
| Puissance p -> Puissance p;
| Exp exp -> Exp (flatten exp)
| a -> a;;

(* Tri des différents termes d'un noeud produit/somme. Permet de simplifier certains termes*)
let sort_op_node_sum l=
 let rec insert a =function (* Insertion dans l'ordre de compare de a dans une liste *)
 | [] -> [a]
 | h::t when a<h ->a::h::t
 | h::t -> h::insert a t in
 let rec insert_rat (bo,a,b)=function (* Liste de taille au plus 2 de la forme (p/q,ip/q), partie réelle en premier *)
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
 | (bo',a',b')::t -> mult_rat (bo,a,b) (bo',a',b')::t in
 let rec sorter=function
 | []->([],[],[],(false,0,1))
 | Node (op,exp_l)::t -> let (l1,exp,cst,pow)=sorter t in (insert (Node (op,exp_l)) l1,exp,cst,pow)
 | Exp expr::t -> let (l1,exp,cst,pow)=sorter t in (l1,insert (expr) exp,cst,pow)
 | Leaf r::t -> let (l1,exp,cst,pow)=sorter t in (l1,exp,insert_rat r cst,pow)
 | Puissance p::t -> let (l1,exp,cst,pow)=sorter t in (l1,exp,cst,(sum_real_rat p pow))
in sorter l
;;