(*#use "topfind";;
#require "graphics";;
open Graphics;;

let draw_maze gr h w =
  open_graph (" "^(string_of_int (20*(w+2)))
             ^"x"^(string_of_int (20*(h+2))));
  set_window_title "Labyrinthe";
  moveto 20 20; lineto (20*(w+1)) 20;
  lineto (20*(w+1)) (20*(h+1));
  lineto 20 (20*(h+1)); lineto 20 20;
  for i=0 to h-2 do
    for j=0 to w-1 do
      let id = i*w+j in
        if not (Graph.exists_edge gr id (id+w)) then
          (moveto (20*(j+1)) (20*(h-i));
           lineto (20*(j+2)) (20*(h-i)))
    done
  done;
  for i=0 to h-1 do
    for j=0 to w-2 do
      let id = i*w+j in
        if not (Graph.exists_edge gr id (id+1)) then
          (moveto (20*(j+2)) (20*(h-i));
           lineto (20*(j+2)) (20*(h-i+1)))
    done
  done;;*)

module Graph : sig
  (* Type abstrait *)
  type 'a t
  (* Creation d'un graphe vide *)
  val create : unit -> 'a t
  val add_edge : 'a t -> 'a -> 'a -> unit
  val del_edge : 'a t -> 'a -> 'a -> unit
  val neighbors : 'a t -> 'a -> 'a list
  val exists_edge : 'a t -> 'a -> 'a -> bool
end = struct
  type 'a t = ('a,'a list) Hashtbl.t
  let create () = Hashtbl.create 97
  let neighbors gr i =
    if not (Hashtbl.mem gr i)
      then []
      else Hashtbl.find gr i
  let exists_edge gr i j =
    List.mem j (neighbors gr i)
  let add_edge gr i j =
    if not (Hashtbl.mem gr i)
      then Hashtbl.add gr i [j]
      else if not (exists_edge gr i j) then 
        Hashtbl.replace gr i (j::Hashtbl.find gr i);
    if not (Hashtbl.mem gr j)
      then Hashtbl.add gr j [i]
      else if not (exists_edge gr j i) then 
        Hashtbl.replace gr j (i::Hashtbl.find gr j)
  let del_edge gr i j =
    if Hashtbl.mem gr i then
      Hashtbl.replace gr i
        (List.filter (fun k -> k<>j) (Hashtbl.find gr i));
    if Hashtbl.mem gr j then
      Hashtbl.replace gr j
        (List.filter (fun k -> k<>i) (Hashtbl.find gr j))
end;;

(*-------------------------------------------------*)
(* Module UnionFind pour gerer des equivalences    *)

module UnionFind : sig
  type 'a t
  val create : unit -> 'a t
  val union : 'a t -> 'a -> 'a -> unit
  val find : 'a t -> 'a -> 'a
  val joined : 'a t -> 'a -> 'a -> bool
end = struct
  type 'a t = ('a,'a) Hashtbl.t
  let create () = Hashtbl.create 97
  let rec _find s i =
    if Hashtbl.mem s i
      then _find s (Hashtbl.find s i) else i
  let find s i =
    let ri = _find s i in
    let rec update i =
      if i = ri then i else
        let j = Hashtbl.find s i
          in Hashtbl.replace s i ri; update j
    in update i
  let joined s i j = find s i = find s j
  let union s i j =
    let ri = find s i and rj = find s j in
      if ri <> rj then Hashtbl.add s ri rj
end;;



let ascii_maze ?(path = []) gr h w =
  for i = 0 to 2*h do
    for j = 0 to 2*w do
      let c = i/2*w+j/2 in
      match i mod 2, j mod 2 with
        | 0,0 -> print_char '+'
        | 0,1 when not (Graph.exists_edge gr c (c-w))
            -> print_char '-'
        | 1,0 when not (Graph.exists_edge gr c (c-1))
            -> print_char '|'
        | 1,1 when List.mem c path
            -> print_char '*'
        | 0,1 when List.mem c path && List.mem (c-w) path
            -> print_char '*'
        | 1,0 when List.mem c path && List.mem (c-1) path
            -> print_char '*'
        | _ -> print_char ' '
    done;
    print_newline ()
  done;;

(*-------------------------------------------------*)

let gen_maze s h w =
  Random.init (2+4*s);
  let gr = Graph.create () in
  let visited = Hashtbl.create 97 in
  let pool = Array.make (h*w*4) (h/2*w+w/2,h/2*w+w/2+1) in
  let swap n = let tmp = pool.(n) and p = Random.int (n+1)
                 in pool.(n) <- pool.(p); pool.(p) <- tmp in
  let rec aux = function
    | -1 -> Graph.del_edge gr ((h/3)*w+w/3) ((h/3)*w+w/3+1); gr
    | n -> let o,i = pool.(n) in let b = Hashtbl.mem visited i in
             if b || Random.int (h*w) = 0
             then aux (n-1) else
               let count = ref n in
                 Hashtbl.add visited i true;
                 Graph.add_edge gr o i;
                 if i mod w <> 0 then
                   (pool.(!count) <- (i,i-1); swap !count; incr count);
                 if i mod w <> (w-1) then
                   (pool.(!count) <- (i,i+1); swap !count; incr count);
                 if i >= w then
                   (pool.(!count) <- (i,i-w); swap !count; incr count);
                 if i < w*(h-1) then
                   (pool.(!count) <- (i,i+w); swap !count; incr count);
                 aux (!count-1);
  in aux 1;;

(*-------------------------------------------------*)

let shuffle_list lst = 
  let tab = Array.of_list lst in
  for i = Array.length tab - 1 downto 1 do
    let j = Random.int (i+1) in
      let tmp = tab.(j) in tab.(j) <- tab.(i); tab.(i) <- tmp
  done;
  Array.to_list tab;;

(*-------------------------------------------------*)


(* test *)
ascii_maze (gen_maze 42 10 15) 10 15;;

let is_connex gr h w =
  let count=ref 0 in
  let dfs a=
    let d=Hashtbl.create 42 in
    let rec visit a=
        Hashtbl.add d a true;
        incr count;
        List.iter (fun v->if not (Hashtbl.mem d v) then (visit v)) (Graph.neighbors gr a)
    in visit a;
  in dfs 0;!count=h*w;;

let print_bool=function
| true -> print_string "true\n"
| _ -> print_string "false\n";;

let is_acyclic gr v w=  
let dfs gr a=
  let d=Hashtbl.create 42 and res=ref true in
  let rec visit lev a=
      if (!res=false) then () else
      List.iter (fun v->if not (Hashtbl.mem d v) then (Hashtbl.add d v (lev+1); visit (lev+1) v) else if (Hashtbl.find d v)<>lev-1 then res:=false) (Graph.neighbors gr a)
  in visit 0 a;!res;
in dfs gr 0;;

let ()=Printf.printf "Réponse question 1 :\n";
  for i=1 to 5 do 
    Printf.printf "%d : %b %b\n" i (is_connex (gen_maze i 30 45) 30 45) (is_acyclic (gen_maze i 30 45) 30 45)
  done;;

let path gr src dst=
let dfs a=
  let d=Hashtbl.create 42 and res=ref [] in
  let rec visit a=
      if (!res<>[]) then () else
      let lst=Hashtbl.find d a in
      List.iter (fun v->if v=dst then res:=dst::lst else if not (Hashtbl.mem d v) then (Hashtbl.add d v (v::lst); visit v)) (Graph.neighbors gr a)
  in Hashtbl.add d a [];visit a;!res;
in dfs src;;

let ()=Printf.printf "Réponse question 6 :\n";
  for i=1 to 5 do 
    Printf.printf "%d : " i;
    List.iter (Printf.printf "|%d") (path (gen_maze i 30 45) 0 1250);
    Printf.printf "|\n";
  done;;

let is_perfect gr v w=is_acyclic gr v w && is_connex gr v w;;

let no_wall h w =
  let gr=Graph.create () in
  for i=0 to h-1 do
    for i'=0 to w-1 do
      (if i'>0 then begin
        Graph.add_edge gr (w*i+i') (w*i+i'-1) end else begin () end);
      (if i>0 then begin
        Graph.add_edge gr (w*i+i') (w*(i-1)+i') end;);
    done;
  done;
  gr;;

let gen_dfs h w=
  let gr=no_wall h w in
  let rec dfs a=
    let d=Hashtbl.create 42 and res=ref [] in
    let rec visit a=
      List.iter (fun v->if not (Hashtbl.mem d v) then (Hashtbl.add d v true; res:=v::a::(!res); visit v)) (Graph.neighbors gr a)
    in Hashtbl.add d a true;visit a;!res
  and iter gr=function
  | a::b::t -> Graph.add_edge gr a b; iter gr (b::t)
  | _ -> gr 
  in let gr_res=Graph.create () in iter gr_res (dfs 0)
;;

let gen_dfs_r h w=
  let gr=no_wall h w in
  let rec dfs a=
    let d=Hashtbl.create 42 and res=ref [] in
    let rec visit a=
      List.iter (fun v->if not (Hashtbl.mem d v) then (Hashtbl.add d v true; res:=v::a::(!res); visit v)) (Graph.neighbors gr a |> shuffle_list)
    in Hashtbl.add d a true;visit a;!res
  and iter gr=function
  | a::b::t -> Graph.add_edge gr a b; iter gr (b::t)
  | _ -> gr 
  in let gr_res=Graph.create () in iter gr_res (dfs 0)
;;

let ()=Printf.printf "Question 9 et 10 :\n"; ascii_maze (gen_dfs 30 45) 30 45; Printf.printf "\n\n";ascii_maze (gen_dfs_r 30 45) 30 45;;

let poss_walls h w =
  let rec build k=match k with
  | k when k>h*w-1 -> []
  | k -> (if (k+1) mod w<>0 then (k,k+1) else (-1,-1)) :: (if k/w>0 then (k,k-w) else (-1,-1)) :: build(k+1)
in List.filter (fun h->h<>(-1,-1)) (build 0);;

let gen_Kruskal h w =
    let possibles=poss_walls h w |> shuffle_list and
    uf = UnionFind.create () and
    g_res=Graph.create () in
    List.iter (fun (i,j)->if UnionFind.joined uf i j then () else begin Graph.add_edge g_res i j;UnionFind.union uf i j end ) possibles;
    g_res;;

let find_cycle_in_connex gr v w=  
let d=Hashtbl.create 42 in
let rec explore v=
  let rec deal_with_neighbours = function
  | [] -> None
  | h::t when Hashtbl.mem d h -> if Hashtbl.find d v=h 
                                  then deal_with_neighbours t 
                                  else begin
                                    Hashtbl.replace d h v;
                                    Some h
                                  end
  | h::t -> Hashtbl.add d h v; match explore h with
                              | None -> deal_with_neighbours t
                              | Some w -> Some w
  in deal_with_neighbours (Graph.neighbors gr v)
in Hashtbl.add d 0 0; match explore 0 with
| None -> None
| Some v ->let rec rebuild = function
           | h when h=v -> [h]
           | h -> h::rebuild (Hashtbl.find d h)
           in Some (rebuild (Hashtbl.find d v)) ;;




let find_cycle gr v w=  
let d=Hashtbl.create 42 in
let rec explore v=
  let rec deal_with_neighbours = function
  | [] -> None
  | h::t when Hashtbl.mem d h -> if Hashtbl.find d v=h 
                                  then deal_with_neighbours t 
                                  else begin
                                    Hashtbl.replace d h v;
                                    Some h
                                  end
  | h::t -> Hashtbl.add d h v; match explore h with
                              | None -> deal_with_neighbours t
                              | Some w -> Some w
  in deal_with_neighbours (Graph.neighbors gr v)
and test k=if Hashtbl.mem d k then test (k+1) else if k=v*w then None else match Hashtbl.add d k k; explore k with
           | None -> test (k+1)
           | Some v ->let rec rebuild = function
                      | h when h=v -> [h]
                      | h -> h::rebuild (Hashtbl.find d h)
                      in Some (rebuild (Hashtbl.find d v))
in test 0;;

let () = Printf.printf "Réponses quant à l'existence de cycles :\n";
  for i=0 to 25 do
    Printf.printf "i=%d\t(%d*%d) : |" i 30 45; List.iter (Printf.printf "%d|") (match find_cycle (gen_maze i 30 45) 30 45 with | Some res -> res | _ -> [-1]);
    Printf.printf "\n";
  done;;

let partition gr h w uf=
  let links=poss_walls 1 w |> List.map (fun (i,j)->i+w*h,j+w*h) in
  List.iter (fun (i,j) -> if UnionFind.joined uf i j |> not && Random.bool () then begin UnionFind.union uf i j; Graph.add_edge gr i j end ) (links |> shuffle_list);
  let rec gen_partition acc=function
  | [] -> acc
  | h::t -> let rec insert =function
            | [] -> [[h]]
            | (a::t')::t'' when UnionFind.joined uf h a -> (h::a::t)::t''
            | h'::t' -> h'::insert t'
            in gen_partition (insert acc) t
  in gen_partition [] (List.init w (fun i->h*w+i));;
let ($) (i,j) (k,l)=(i+k,j+l);;

(* Cette fonction présente des défauts : notamment, elle n'affiche par un graphe parfait et la ligne du bas n'a jamais de mur latéral..*)
let gen_Eller h w=
  let uf=UnionFind.create () and gr=Graph.create () in
  let rec treat_classes=function
  | [] -> ()
  | (a::t')::t -> Graph.add_edge gr a (a+w);
            let rec treat_lone_class=function
            | [] -> treat_classes t
            | h::t'' -> if Random.bool () then Graph.add_edge gr h (h+w) else begin () end; treat_lone_class t''
            in treat_lone_class t'
  | []::t -> failwith "Corrupt partition"
  in
  for i=0 to h-2 do
    let classes=partition gr i w uf in
    treat_classes classes;
  done;
  let links=poss_walls 1 w |> List.map (fun (i,j) -> (i,j)$((h-1)*w,(h-1)*w)) |> shuffle_list in
  List.iter (fun (i,j) ->if UnionFind.joined uf i j |> not then begin UnionFind.union uf i j; Graph.add_edge gr i j end) links;
  gr;;
