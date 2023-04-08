(* 
Edouard Aubert 2023
Roadmap :
+ Vérifier que la chaîne entrée par l'utilisateur ne contienne rien d'autres que des lettres et trois jokers
+ Implémenter une structure d'arbre si la structure de liste anti-collision ne suffit pas en termes de performances
+ Quelques optimisations de base à implémenter (chaîne de longueur inférieure à 30 caractères ?)

*)

type btree=Node of btree*string*btree | Leaf ;;

(* Fonctions pour manipuler un arbre binaire de recherche *)
(*let rec insert v = function
| Leaf -> Node (Leaf,v,Leaf)
| Node (c1,l,c2) -> if v<l then Node(insert v c1,l,c2) else Node (c1,l,insert v c2)
;;*)


let rec height = function
| Leaf -> 0
| Node (c1,_,c2) -> (max (height c1) (height c2)) + 1;;

let rec insert_rot v = function
| Leaf -> Node (Leaf,v,Leaf)
| Node (c1,l,c2) when height c1>=height c2 -> Node (c1,l,insert_rot v c2)
| Node (c1,l,Node (c21,l2,c22)) -> Node (Node (c1,l,c21),l2,insert_rot v c22)
| _ -> failwith "should not happen..";;

let rec size = function
| Leaf -> 0
| Node (c1,_,c2) -> (size c1)+(size c2) + 1;;

(* Variable globale donnant le nombre de mots inclus dans le fichier *)
let dico_size =let f = open_in "dict.dat" in
  let res=int_of_string (input_line f) in 
  close_in f; res;;

(* Lit le fichier "dict.dat" et construit une liste de chaînes
   appelée words_list contenant l'ensemble des mots considérés valides *)
let words_list =
  let f = open_in "dict.dat" in
    let rec aux lst n =
      if n=0 then List.rev lst else aux ((input_line f)::lst) (n-1)
    in aux [] (int_of_string (input_line f));;

(* Prend une requête (chaine de caractères) et affiche les mots possibles *)
let find req = 
  List.iter (Printf.printf "%s\n") (List.filter ((=) req) words_list)

(* Test *)
let ()=match List.length words_list with
| l when l=dico_size -> ()
| _ -> failwith "Test failed : list size does not match dico_size";;

(* Solution 1 : Tableau de hachage indexé selon les indices pairs (en partant de 0) de caractères *)
let c_code s i=if i<String.length s then Char.code (s.[i])-97 else 0;;

let remain=function
| n when n>8 -> 0
| n -> -2+(n mod 2)+((28.**(((10-n)/2)|> float_of_int)) |> int_of_float);;

let control str = if String.length str>15 then false else String.fold_left (fun prev h -> prev && ((Char.code h)>=97 && (Char.code h)<=122 || h='?')) true str;;

let hash=
  let arr=Array.make (26*28*28*28*28) (Leaf) in
  let rec hash_aux gr_m=function
  | [] -> Printf.printf "Highest tree : %d\n" gr_m; ()
  | str::t -> match String.length str with
              | 0 -> failwith "Corrupt file input (word of size 0 !)"
              | n -> arr.((c_code str 0)*28*28*28*28+(c_code str 2)*28*28*28+(c_code str 4)*28*28+(c_code str 6)*28+(c_code str 8)+remain n) <- insert_rot str arr.((c_code str 0)*28*28*28*28+(c_code str 2)*28*28*28+(c_code str 4)*28*28+(c_code str 6)*28+(c_code str 8)+remain n);
                      hash_aux (max gr_m (height (arr.((c_code str 0)*28*28*28*28+(c_code str 2)*28*28*28+(c_code str 4)*28*28+(c_code str 6)*28+(c_code str 8)+remain n)))) t;
in hash_aux 0 words_list; arr;;

exception Not_found;;

let find_2 key= 
              let rec find_in_b_tree key =function
              | Leaf -> false
              | Node (_,h,_) when h=key -> true
              | Node (_,h,c2) when h<key -> find_in_b_tree key c2
              | Node (c1,_,_) -> find_in_b_tree key c1
              in
              find_in_b_tree key hash.((c_code key 0)*28*28*28*28+(c_code key 2)*28*28*28+(c_code key 4)*28*28+(c_code key 6)*28+(c_code key 8)+remain (String.length key))
;;

let rec remove el=function
| [] -> Printf.printf "Warning : Could not remove element you asked for (it is not inside the list).\n"; []
| (v,1)::t when v=el -> t
| (v,l)::t when v=el -> (v,l-1)::t
| h::t -> h::remove el t
;;

let to_duplicounts lst =
  let rec aux acc = function
  | [] -> acc
  | h::t -> let rec insert el=function
            | (v,k)::t when el=v -> (v,k+1)::t
            | h::t -> h::insert el t
            | [] -> [(el,1)]
            in aux (insert h acc) t
  in aux [] lst
;;

let search_combinations char_list=
    (* Implementation sans les points d'interrogation *)
    let rec generate_cases acc =function
    | [] -> if find_2 acc then Printf.printf "%s\n" acc else ()
    | lst -> List.iter (fun v->generate_cases (acc^(String.make 1 (fst v))) (remove (fst v) lst)) lst
in generate_cases "" (char_list |> to_duplicounts)
;;

let joker_cases str=
let char_list=(str |> String.to_seq |> List.of_seq) in
let jokers=List.fold_left (fun acc h -> if h='?' then acc+1 else acc) 0 char_list in
let list_without_wildcards=List.filter (fun h -> h<>'?') char_list in
match jokers with
| 0 ->  search_combinations char_list;
| 1 -> for i=0 to 25 do search_combinations ((char_of_int (97+i))::list_without_wildcards) done;
| 2 -> for i=0 to 25 do
          for j=i to 25 do
            search_combinations ((char_of_int (97+i))::(char_of_int (97+j))::list_without_wildcards)
          done;
       done;
| 3 -> for i=0 to 25 do
        for j=i to 25 do
          for k=j to 25 do
            search_combinations ((char_of_int (97+i))::(char_of_int (97+j))::(char_of_int (97+k))::list_without_wildcards)
          done;
        done;
       done;
| _ -> Printf.printf "There are too much wildcards (cannot exceed 3) !\n";
;;

let search str = if not (control str) then () (* Si on est dans ce cas là, on est certain de ne pas contenir de tel mot *)
            else joker_cases str;;

(* Boucle principale *) 
let () =
  let rec loop () =
    let req = print_string "Lettres : "; read_line () in
      if req = "!" then () else (search req; loop ())
    in loop ()
;;

  
