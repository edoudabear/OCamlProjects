type base = A | C | G | T;;

type trie = Nil | Node of bool * trie * trie * trie * trie;;

let rec count = function
| Nil -> 0
| Node (true,a,b,c,d) -> 1 + count a + count b + count c + count d
| Node (false,a,b,c,d) -> count a + count b + count c + count d;;


let isBlack=function
| Node (v,_,_,_,_) -> v
| Nil -> false;;

let subtree nuple k = match nuple with
| Node (a,b,c,d,e) -> begin match k with
                | A -> b
                | C -> c
                | G -> d
                | T -> e
end
| Nil -> Nil
;;

let rec mem base tree= if base=[] then
                              if tree<>Nil && (isBlack tree) then true else false  
                           else match List.hd base with
| A -> if tree=Nil then false else mem (List.tl base) (subtree tree A)
| C -> if tree=Nil then false else mem (List.tl base) (subtree tree C)
| G -> if tree=Nil then false else mem (List.tl base) (subtree tree G)
| T -> if tree=Nil then false else mem (List.tl base) (subtree tree T);;

let rec subst base a s=match a with
| Node (v,b,c,d,e) -> Node (v,
                       (if base=A then s else b),
                       (if base=C then s else c),
                       (if base=G then s else d),
                       (if base=T then s else e))
| Nil -> Node (false,
(if base=A then s else Nil),
(if base=C then s else Nil),
(if base=G then s else Nil),
(if base=T then s else Nil));;

let rec add base tree = match base,tree with
| [],Nil -> Node (true,Nil,Nil,Nil,Nil)
| [],Node (_,b,c,d,e) -> Node (true,b,c,d,e)
| h::t,_ -> subst h tree (add t (subtree tree h));; 

(*let rec add base a= if base=[] then Nil else match List.hd base,a with
| A,Nil -> Node (List.tl base=[],add (List.tl base) Nil,Nil,Nil,Nil)
| C,Nil -> Node (List.tl base=[],Nil,add (List.tl base) Nil ,Nil,Nil)
| G,Nil -> Node (List.tl base=[],Nil ,Nil,add (List.tl base) Nil,Nil)
| T,Nil -> Node (List.tl base=[],Nil,Nil,Nil,add (List.tl base) Nil)
| A,Node (v,b,c,d,e) -> Node (List.tl base=[],add (List.tl base) b,c,d,e)
| C,Node (v,b,c,d,e) -> Node (List.tl base=[], b,add (List.tl base) c,d,e)
| G,Node (v,b,c,d,e) -> Node (List.tl base=[],b,c,add (List.tl base) d,e)
| T,Node (v,b,c,d,e) -> Node (List.tl base=[],b,c, d,add (List.tl base) e);;*)

let adn=Node (false,
Node(false,Node (true,Nil,Nil,Nil,Nil),Node (true,Nil,Nil,Nil,Nil),Nil,Nil),
Node(true,Nil,Nil,Nil,Nil),
Node(true,Node(true,Nil,Nil,Nil,Nil),Nil,Nil,Nil),
Nil);;

let rec trie_of_list=function
| [] -> Nil
| h::t -> add h (trie_of_list t);;

let rec depth = function
| Nil -> 0
| Node (_,a,b,c,d) -> max (max (depth a) (depth b)) (max (depth c) (depth d));;

let rec list_of_trie=function
| Nil -> []
| Node (v,a,b,c,d)-> (if v then [[]] else [])@
(List.map (fun h->A::h) (list_of_trie a))@(List.map (fun h->C::h) (list_of_trie b))@(List.map (fun h->G::h) (list_of_trie c))@(List.map (fun h->T::h) (list_of_trie d));;

let print_base b =
   print_char ((function | A -> 'A' | C -> 'C' | G -> 'G' | T -> 'T') b);;

let print_fragm l=List.iter print_base l;Printf.printf "\n";;

let display t=
   let rec aux acc=function
   | Nil -> ()
   | Node (v,a,b,c,d) ->if v then print_fragm (List.rev acc); aux (A::acc) a;aux (C::acc) b; aux (G::acc) c;aux (T::acc) d 
   in aux ([]) t;;

let sort t= t|> trie_of_list |> list_of_trie;;

let rec delete base =function
| Nil when base<>[]->failwith "Invalid base list"
| Nil -> Nil
| Node (v,a,b,c,d) when base=[] -> Node (false,a,b,c,d)
| Node (v,a,b,c,d) -> begin match List.hd base with
                     | A -> Node (v,delete (List.tl base) a,b,c,d)
                     | C -> Node (v,a,delete (List.tl base) b,c,d)
                     | G -> Node (v,a,b,delete (List.tl base) c,d)
                     | T -> Node (v,a,b,c,delete (List.tl base) d) 
                  end;;

let rec is_empty=function
| Nil -> false  (* cas évident*)
| Node (v,a,b,c,d) -> v || is_empty a || is_empty b || is_empty c || is_empty d;;

let rec is_compact=function
| Nil -> true
| Node (v,Nil,Nil,Nil,Nil) -> v
| Node (v,a,b,c,d) -> is_compact a && is_compact b && is_compact c && is_compact d;;

let rec cut = function
| Nil -> Nil
| Node (v,Nil,Nil,Nil,Nil) -> if v then Node (v,Nil,Nil,Nil,Nil) else Nil
| Node (v,a,b,c,d) -> match cut a,cut b,cut c, cut d with
                     | Nil,Nil,Nil,Nil -> if v then Node (v,Nil,Nil,Nil,Nil) else Nil
                     | a',b',c',d' -> Node (v,a',b',c',d');;

type compr_trie = CNode of base list*bool*compr_trie list;;

let rec is_prefix b1 b2=match b1,b2 with
| [],[] -> true
| h1::t1,h2::t2 -> if h1=h2 then is_prefix t1 t2 else false
| _,[] -> false
| [],_ -> true
;;

let rec del k=function
| l when k=0 -> l
| [] -> []
| h::t when k=1 -> t
| h::t -> del (k-1) t;;

let rec print_base_list =function
| [] -> Printf.printf "\n"
| h::t -> print_base h;print_base_list t;;

let rec compr_mem tree v=match tree with
| CNode (l1,bo,_) when l1=v-> bo
| CNode (l1,bo,_) when not (is_prefix l1 v) -> false
| CNode (l1,_,subt_l) -> let new_v=del (List.length l1) v
      in List.fold_left (fun a subt-> a || compr_mem subt new_v) (false) subt_l
;;

let adn2=CNode ([],false,[
   CNode ([A;A;T],false,[
      CNode ([C],true,[]);
      CNode ([G;C],true,[])
   ]);
   CNode ([G;C],true,[
      CNode([T],false,[
         CNode ([C],true,[]);
         CNode ([T;A],true,[])
      ])
   ])
]);;

let rec list_of_compr_trie = function
| CNode (a,true,[])->[a]
| CNode (a,false,v)->List.fold_left (fun acc n_l->acc@(List.map (fun el->a@el) (list_of_compr_trie n_l))) [] v
| CNode (a,true,v) -> [a]@List.fold_left (fun acc n_l->acc@(List.map (fun el->a@el) (list_of_compr_trie n_l))) [] v;;

let rec common l1 l2=match l1,l2 with
| [],[] | [],_ | _,[] -> []
| h1::t1,h2::t2 when h1<>h2 -> []
| h1::t1,h2::t2 -> h1::common t1 t2;;

(*let compr_add pt b_l=
   match pt with
   | CNode ([],false,[]) -> CNode (b_l,true,[])
   | CNode (a,v,t) when not (is_prefix a b_l) -> if is_prefix b_l a then CNode (b_l,true,[CNode (del (List.length b_l) v,true,t)]) else let c=common a b_l in CNode (c,false,[CNode (del (List.length c) b_l,true,[]);CNode (del (List.length c) a,v,t)])
   | pt -> begin let rec aux_add b_l_i = function
               | CNode (a,v,t) when (List.fold_left (fun a b->a || is_prefix b_l_i b) false t) -> CNode (a,v,List.map (function | CNode (a',v',t') when if is_prefix a' b_l_i -> CNode (a',v',compr_add  (CNode (a',v',t')) (delete (List.length a') b_l_i) ) | x-> x ))
               | _ -> CNode([],true,[])
               end
   if pt=CNode ([],false,[]) then CNode (b_l,true,[]) else
   if pt=*)