let n_max=ref (0);;
let u_0=1673;;
(*let u_0=486;;*)
Printf.printf "SEED : %d\n" u_0;;

let u_save=Array.make (64007) (u_0);;
let u =function
| k when k<=(!n_max) -> u_save.(k)
| 0 -> u_save.(0)<-u_0;u_0
| k -> let rec iter =function
       | l when l=k->u_save.(k)
       | l -> u_save.(l+1)<-(15091*u_save.(l)) mod 64007; iter (l+1)
       in iter !n_max
;;

let g n i=(u (10*n+2*i) mod (n-1)) + 1;;
let d n i=(u (10*n+2*i+1) mod (n-1)) + 1;;
let t_Eq n i=(g n i,(d n i-g n i,n));;

(* Question 1 : réponse sous la forme (y,(a,b)) où y est l'ordonnée à l'origine et a/b le coeff. directeur : *)
let ()=Printf.printf "Question 1:\n";for i=0 to 2 do
    let y,(a,b)=t_Eq 100 i in Printf.printf "%d : %d/%dx+%d\n" i a b y
done;; 

(* Question oral 1 : on peut vérifier en temps linéaire en N cela (en supposant l'accès aux termes de la suite comme étant constant)
 en itérant sur les différents segments, en retenant leurs indices gauche et droit et en les stockant comme clés dans un dictionnaire.
 Tant qu'on ne trouve pas de paire et qu'on n'a pas testé tous les segments, on vérifie si le nouveau segment testé est déjà clé du dictionnaire,
 ce qui signifierait que cette propriété d'unicité n'est pas vérifiée. Sinon, on l'ajoute en tant que clé sous forme de couple dans le dictionnaire.
 
 Mots clés : linéaire en la taille de N, dictionnaire *)

(*Question oral 2 :
  Pour déterminer la couleur d'une surface, on peut considérer le nombre de segments traversés en descendans de l'axe du haut, y compris lorsqu'on passe sous deux intersections

  L'unicité provient du fait que si on colorie uniquement de deux couleurs, alors on n'a jamais deux possibilités lors de la 
  construction du coloriage (si on considère un arbre de décision pour construire un coloriage bicolore, on aurait une chaîne).
   
  NB : évidemment, dans ce raisonnement, on considère que le coloriage inversé est identique au coloriage de base (donc le choix de la couleur de départ
  n'est pas un choix en soi, c'est pourquoi le sujet suggère qu'on commence par 1 pour le point d'ordonnée 0).  
*)
let eval_rat n i (xa,xb)=
  let (y_0,(xn,xd))=t_Eq n i in
  (y_0*xd*xb+xa*xn,xd*xb);;

exception DivBy0;;
let cmp (a,b) (c,d)=
  if (b<0 && d<0 || d>0 && b>0) then
    if (a*d>b*c) then 1 else if (a*d<b*c) then -1 else 0
  else if (b>0 && d<0 || b<0 && d>0) then
    if (a*d>b*c) then -1 else if (a*d<b*c) then 1 else 0
  else raise DivBy0;;
    

exception IsOnALine;;
let couleur_opti n (xa,xb) (ya,yb)=
  let rec iter=function
  | k when k=n -> 0
  | k -> let cmpVal=cmp (eval_rat n k (xa,xb)) (ya,yb) in (if cmpVal=(-1) then 1 else if cmpVal=1 then 0 else raise IsOnALine) + iter (k+1)
  in try (iter 0 mod 2) +1 with | IsOnALine -> 0;;

(* Question 2 : On compte le nombre de droites au dessus du point (si le point est sur une droite, on déclenche l'alarme IsOnALine)*)
let ()=Printf.printf "Question 2 :\n";List.iter (fun v->Printf.printf "n=%d, %d\n" v (couleur_opti v (78*v,139) (78*v,139))) [10;20;50];;

(* Question 3 : On utilise le fait que cela impose que un segment intersectant avec un autre doit avoir une extrémité gauche ou droite au dessus de l'autre segment et l'autre en dessous*)

exception InvalidOperator;;
let op (a,b) (c,d) operator=if operator='+' then (a*d+c*b,d*b) else if operator='-' then (a*d-c*b,d*b) else raise InvalidOperator
let sg (a,b)=if a=0 then 0 else a*b/(abs (a*b) );;

let n_pairs n=
  let rec iter = function
  | (i,j) when i=n-1 && j=n -> 0
  | (i,j) when j=n ->iter (i+1,i+2)
  | (i,j) -> let g=op (eval_rat n i (0,1)) (eval_rat n j (0,1)) '-' 
             and d=op (eval_rat n i (n,1)) (eval_rat n j (n,1)) '-' 
              in (if sg g*sg d=(-1) then 1 else 0) + iter (i,j+1)
in iter (0,1);;

let ()=Printf.printf "Question 3 :\n"; List.iter (fun v->Printf.printf "n=%d, N_noeuds=%d\n" v (n_pairs v)) [10;20;50];;


(* Question 4 : On réutilise l'itération de la question précédente *)

let coords_crossing n i j=
  let (yi,(xi1,xi2))=t_Eq n i and (yj,(xj1,xj2))=t_Eq n j in
  (* yi-yj+x(xi1/xi2-xj1/xj2)=0 i.e. x=(yj-yi)/(xi1/xi2-xj1/xj2) i.e. (yj-yi)*(xi2*xj2)/(xi1*xj2-xj1*xi2) *)
  let x=((yj-yi)*(xi2*xj2),(xi1*xj2-xj1*xi2)) in
  (x,eval_rat n i x);;

let min_cross n=
  let minV=ref ((-1,-1),(-1,-1)) in
  let rec iter = function
  | (i,j) when i=n-1 && j=n -> ()
  | (i,j) when j=n ->iter (i+1,i+2)
  | (i,j) -> let g=op (eval_rat n i (0,1)) (eval_rat n j (0,1)) '-' 
             and d=op (eval_rat n i (n,1)) (eval_rat n j (n,1)) '-' 
              in (if sg g*sg d=(-1) then begin let m=coords_crossing n i j in if (!minV=((-1,-1),(-1,-1)) || cmp (fst !minV) (fst m)=1 || (cmp (fst !minV) (fst m)=0 && cmp (snd !minV) (snd m)=1) ) then minV:=m end) ; iter (i,j+1)
in iter (0,1); let (x1,x2),(y1,y2)=(!minV) in let x=if sg (x1,x2)=(-1) then -abs x1,abs x2 else abs x1,abs x2
                                            and y=if sg (y1,y2)=(-1) then -abs y1,abs y2 else abs y1,abs y2 in (x,y);;

let ()=Printf.printf "Question 4 :\n";List.iter (fun v->let (x1,x2),(y1,y2)=(min_cross v) in Printf.printf "n=%d, x=%d/%d y=%d/%d\n" v x1 x2 y1 y2) [10;20;50];;

(* Question 5 : utiliser un dictionnaire et un algo pour obtenir la forme irréductible d'un quotient*)

let rec gcd a b=match abs a,abs b with
| a',0 | 0,a' -> a'
|a',b'->gcd (max a' b' mod min a' b') (min a' b');;

let reduce (xn,xd)=
  let k=gcd xn xd in if sg (xn,xd)=(-1) then (-abs (xn/k),abs (xd/k)) else (abs (xn/k),abs (xd/k));; 

let n_nodes n=
  let dict=Hashtbl.create 16 in
  let rec iter = function
  | (i,j) when i=n-1 && j=n -> ()
  | (i,j) when j=n ->iter (i+1,i+2)
  | (i,j) -> let g=op (eval_rat n i (0,1)) (eval_rat n j (0,1)) '-'
             and d=op (eval_rat n i (n,1)) (eval_rat n j (n,1)) '-'
              in if sg g*sg d=(-1) then begin 
                let x,y=coords_crossing n i j in
                let x,y=reduce x,reduce y in
                if Hashtbl.mem dict (x,y) |> not then
                  Hashtbl.add dict (x,y) true;
              end ;iter (i,j+1)
in iter (0,1);Hashtbl.length dict;;

let ()=Printf.printf "Question 5 :\n" ;List.iter (fun v->Printf.printf "n=%d, N_noeuds=%d\n" v (n_nodes v)) [10;20;50];;

(* Complexité spatiale en O(N^2) (on a au plus N(N+1)/2 noeuds, qui sont comptés dans un dictionnaire)
   Complexité temporelle en O(N^2*k^3) (où k est le nombre de bits, d'un entier, d'après wikipédia)
   ou O(N^2*2log(2^63)*)

(* Question 6 : méthode constructive, pas inspirée de la suggestion de l'énoncé.. *)
let n_sectors n=
  let count=ref 1 and dict=Hashtbl.create 16 in
  let rec iter = function
  | (i,j) when i=n-1 && j=n -> ()
  | (i,j) when j=i+1 ->Hashtbl.clear dict ;incr count;iter (i+1,0) (* Clear > reset car évite de reshrinker la table de hashage *)
  | (i,j) -> let g=op (eval_rat n i (0,1)) (eval_rat n j (0,1)) '-'
             and d=op (eval_rat n i (n,1)) (eval_rat n j (n,1)) '-'
              in if sg g*sg d=(-1) then begin 
                let x,y=coords_crossing n i j in
                let x,y=reduce x,reduce y in
                if (Hashtbl.mem dict (x,y) |> not) then begin
                  Hashtbl.add dict (x,y) true; incr count;
                end
              end ;iter (i,j+1)
in iter (0,0);incr count; !count;;

let ()=Printf.printf "Question 6 :\n" ;List.iter (fun v->Printf.printf "n=%d, n_secteurs=%d\n" v (n_sectors v)) [10;20;50];;


let compare_el (a1, a2) (b1,b2)=sg (a2,b2)*compare (a1*b2) (b1*a2);;

let n_crosses n=
  let dict=Hashtbl.create 16 in
  let rec iter = function
  | (i,j) when i=n-1 && j=n ->()
  | (i,j) when j=n ->iter (i+1,i+2)
  | (i,j) -> let g=op (eval_rat n i (0,1)) (eval_rat n j (0,1)) '-'
             and d=op (eval_rat n i (n,1)) (eval_rat n j (n,1)) '-'
              in if sg g*sg d=(-1) then begin 
                let x,y=coords_crossing n i j in
                let x,y=reduce x,reduce y in
                if Hashtbl.mem dict (x,y) |> not then begin
                  Hashtbl.add dict (x,y) ([i;j]);
                end else
                  let n_segments=ref [] and visited=Hashtbl.find dict (x,y) in
                  if (List.mem i visited) then
                    n_segments:=i::(!n_segments);
                  if (List.mem j visited) then
                    n_segments:=i::!n_segments;
                  Hashtbl.replace dict (x,y) (!n_segments@visited)
              end;
              iter (i,j+1)
in iter (0,1);let res=ref [] in
Hashtbl.iter (fun k l-> res:=(k,List.length l)::!res) dict;
List.sort (fun (((a1, a2), (_, _)),_) (((b1,b2),(_,_)),_) ->compare_el (a1,a2) (b1,b2)) !res;;

let sum_areas n=List.fold_left (fun hr ((_,_),l)->hr+2*l) 0 (n_crosses n);;

(* Question 7 *)
let find_area num denom n=
  let rec iter k=function
  | []->k
  |(((x_n,x_d),_),_)::t ->if compare_el (n*num,denom) (x_n,x_d)=1 then iter (k+1) t else k
and d=Hashtbl.create 16
in iter 1 (List.filter (fun ((v,_),_)->if Hashtbl.mem d v then false else (Hashtbl.add d v true; true)) (n_crosses n));;

let ()=Printf.printf "Question 7 :\n" ;List.iter (fun v->Printf.printf "n=%d, secteur_associé=%d\n" v (find_area 78 139 v)) [10;20;50];;

(* Question 8 : on utilise le fait qu'il y a exactement (N+1) secteurs par tranche *)


(*
let associated_sector n num denom=
  let rec next_chunk chunk_abs_l l=if compare_el (List.hd chunk_abs_l) (n*num,denom)>0 then () else next_chunk (List.tl chunk_abs_l) (update l List.hd chunk_abs_l)
  and update l k=
*)

let gen_seg_eqs n=List.init n (fun k->t_Eq n k);;

let eval_eq (y0,(a,b)) (p,q)=op (y0,1) (a*p,b*q) '+';; (* Signe à vérifier *)

let sort_eqs seg_eqs x_m x_M=List.sort (fun se_1 se_2->let cmp=compare_el (eval_eq se_1 x_m) (eval_eq se_2 x_m) in if cmp<>0 then cmp else compare_el (eval_eq se_1 x_M) (eval_eq se_2 x_M)) seg_eqs ;; (* Tri par ordre lexicographique des équations selon (x_m,x_M)*)

let rec del_dups = function
| [] -> []
| h::t when List.mem h t -> del_dups t
| h::t -> h::del_dups t;;

let nodes_abs n=
  let dict=Hashtbl.create 16 in
  let rec iter = function
  | (i,j) when i=n-1 && j=n -> ()
  | (i,j) when j=n ->iter (i+1,i+2)
  | (i,j) -> let g=op (eval_rat n i (0,1)) (eval_rat n j (0,1)) '-'
             and d=op (eval_rat n i (n,1)) (eval_rat n j (n,1)) '-'
              in if sg g*sg d=(-1) then begin 
                let x,y=coords_crossing n i j in
                let x,y=reduce x,reduce y in
                if Hashtbl.mem dict (x,y) |> not then
                  Hashtbl.add dict (x,y) true;
              end ;iter (i,j+1)
in iter (0,1);
let res=ref [] in Hashtbl.iter (fun (x,_) _-> res := x::(!res)) dict;!res |> del_dups |> List.sort (compare_el);;

let to_first n l=
  let rec aux k=function
  | [] -> []
  | [h] -> [(h,(n,(0,1)),k)]
  | h1::h2::t -> (h1,h2,k)::aux (k+1) (h2::t)
in ((0,(0,1)),List.hd l,1)::(aux 2 l);;


(*
let rec iter_sectors n k_0 previous_sectors seg_eqs abs_l=
  let x_m,x_M=match abs_l with
  | [] -> failwith "Invalid abs_list"
  | [a] ->a,(n,1)
  | a::b::t -> a,b
and update_sectors=function
  | [],_ | _,[] -> []
  | h::t,h'::t' when snd h=h'-> h::update_sectors (t,t')
  | h::t,h'::t' -> 
in sort_eqs seg_eqs x_m x_M
*)


