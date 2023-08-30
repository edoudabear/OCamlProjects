let display n = 
  List.iter (fun i ->
    let rec aux = function
    | 0 -> print_newline ()
    | j -> print_char (if i+j=n then 'X' else '.');aux (j-1)
  in aux n;);;

let conflit (i1,j1) (i2,j2) = i1=i2 || j1=j2 || abs (i2-i1)=abs (j2-j1);;

let verifie i_l=
  let rec convert l =
    let rec convert_aux i=function
    | [] -> []
    | h::t -> (i,h)::convert_aux (i+1) t
  in convert_aux 0 l
  and check l= 
    if l=[] then true else
    let rec check_aux v=function
    | [] when l=[] -> true
    | [] -> check (List.tl l)
    | h::t -> if conflit v h then false else check_aux v t
    in check_aux (List.hd l) (List.tl l)
  in check (convert i_l);;

let rec suivant n = function
| [] -> None
| h::t when h=n-1 -> begin match suivant n t with | Some v-> Some (0::v) | None -> None end
| h::t -> Some ((h+1)::t);;

let test k=
  let rec test_iter = function
  | None -> None
  | Some l -> if verifie l then Some l else test_iter (suivant k l)
in test_iter (Some (List.init k (fun _ -> 0)));;

let possibles n i_l =
  let rec possibles_aux= function
  | -1 -> []
  | k -> let res=possibles_aux (k-1) in if verifie (k::i_l) then k::res else res
in possibles_aux (n-1);;

let rec explore n k lst = match k with
| k when k=n -> Some lst
| k -> begin let rec aux =function
       | [] -> None
       | h::t -> begin match explore n (k+1) (h::lst) with
                 | None -> aux t
                 | Some v -> Some v
                end
        in aux (possibles n lst)
      end;;

let resoud n = explore n 0 [];;
(* Efficace jusqu'à n=28 *)
(*
let ()=match resoud (read_int ()) with
      | None -> Printf.printf "None\n"
      | Some list -> List.iter (fun v -> Printf.printf "|%d|" v) list;print_char '\n' ;;
*)

let init k=let arr=Array.make_matrix (k) (k) (-1) in arr.(0).(0)<-0; arr ;;

let affiche =
  Array.iter (fun lgn ->
    Array.iter (fun x -> Printf.printf "%4d " x) lgn;
    print_newline ());;

let possibles arr (i,j)=
  let n=Array.length arr-1 in
  let rec iterator = function
  | [] -> []
  | (k,l)::q -> if k>n || l>n || k<0 || l<0 || (arr.(k).(l))<>(-1) then iterator q else (k,l)::iterator q
  in iterator [(i-1,j+2);(i-1,j-2);(i+1,j+2);(i+1,j-2);(i-2,j-1);(i+2,j-1);(i-2,j+1);(i+2,j+1)];;

let sq x=x*x;;

let rec explore2 fct_ord_possibilites k (i,k) grid=
  grid.(i).(k)<- k; 
  match k with
  | k when k=sq (Array.length grid)-1 -> Some grid
  | k -> let rec explore2_aux=function
         | [] -> None
         | (a,b)::t -> begin match explore2 fct_ord_possibilites (k+1) (a,b) grid with
                   | Some grid -> Some grid
                   | None -> grid.(a).(b)<-(-1);explore2_aux t
                   end
          in explore2_aux (possibles grid (i,k));;

(* prend en argument une fonction : soit possibles, possibles_decroissant, possible_croissant*)
let tour fct_ord_possibilites n=explore2 fct_ord_possibilites 0 (0,0) (init n);;

let heur_decroissant grid (i,j)=8-List.length (possibles grid (i,j));;

let ordonne heur board =
  List.sort (fun x y -> compare (heur board y) (heur board x));;

let possibles_decroissant grid (i,j)=ordonne heur_decroissant grid;;

let heur_croissant grid (i,j)=List.length (possibles grid (i,j));;

let possibles_croissant grid (i,j)=ordonne heur_croissant grid;;

let heur_dist grid (i,j)=
  let n=Array.length grid -1 in 8-min (min i (n-1-i)) (min j (n-1-j));;

let possibles_dist grid (i,j)=ordonne heur_dist grid;;