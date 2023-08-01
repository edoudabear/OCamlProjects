(* Types pour les fichiers binaires *)

type outbitfile = {
  file : out_channel;
  mutable buff : int };;

type inbitfile = {
  data : string;
  mutable pos : int };;

(* Fonctions pour les fichiers *)

let open_bit_out filename =
   { file = open_out filename; buff = 1 };;

let output_bit fd b =
  fd.buff <- fd.buff*2 + (if b then 1 else 0);
  if fd.buff >= 64 then
    begin
      output_string fd.file (String.make 1 (char_of_int (fd.buff-32)));
      fd.buff <- 1;
    end;;

let close_bit_out fd =
  while fd.buff <> 1 do output_bit fd false done;
  close_out fd.file;;

(* Fonctions pour la relecture *)

let open_bit_in filename =
  let fd = open_in filename in
  let data = input_line fd in
    close_in fd; { data = data; pos = 0 };;

let read_bit fd =
  let i = fd.pos / 6 in
  let k = int_of_char (fd.data.[i]) - 32 in
  let b = k land (1 lsl (5 - fd.pos mod 6)) <> 0 in
  fd.pos <- fd.pos + 1; b;;

(* Fonctions pour lire le contenu d'un fichier texte *)

let read_file filename = 
  let f = open_in filename in
  let txt = ref [] in
    try
      while true do txt := input_line f :: !txt done; ""
    with End_of_file -> String.concat "\n" (List.rev !txt);;

(* Encodage de Huffman *)

type tree = Leaf of int*int | Node of int*tree*tree;;

let char_counts str=
  let res=Array.make 256 0 in
  String.iter (fun c->res.(int_of_char c)<-res.(int_of_char c)+1) str;res;;

let rec nb_codes=function
| Leaf (_,_) -> 1
| Node (_,a,b) -> nb_codes a+nb_codes b;;

let to_list arr=
    let res=ref [] in
    Array.iteri (fun i k->if k<>0 then res:=(Leaf (i,k))::(!res)) arr;
    (Leaf (-1,1))::!res;;

exception Empty_List;;

let value=function
| Node (k,_,_) -> k
| Leaf (_,k) -> k;;

let rec smallest=function
| [] -> raise Empty_List
| [s] -> s
| s'::t -> let s=smallest t in if value s<value s' then s else s'
;;

let extract_smallest l=
    try
    let rec pluspetit=smallest l and
    delete v=function
    | [] -> []
    | h::t when h=v -> t
    | h::t -> h::delete v t
  in delete pluspetit l,pluspetit with
  Empty_List ->Printf.printf "Rose exception";raise Empty_List;;

let join a b=Node (value a+value b,a,b);;

let build_huffman_tree arr=
    let leaf_list=arr |> to_list in
    let rec build_tree l=match l with
    | [a] -> a
    | l -> let new_list,s_el=extract_smallest l in 
    let s_el=s_el and new_list,second_small=extract_smallest new_list in
    build_tree ((join s_el second_small)::new_list)
  in build_tree leaf_list;;

let data=char_counts ("adn.dat" |> read_file);;

let tree_to_dict tr=
  let d=Hashtbl.create 16 in
  let rec iter_tree acc=function
  | Leaf (code,_) ->Hashtbl.add d code (List.rev acc)
  | Node (k,a,b) -> iter_tree (false::acc) a;iter_tree (true::acc) b
in iter_tree [] tr;d;;

let ratio str=
  let arr=str |> char_counts in
  Printf.printf "%d/%d" (Hashtbl.fold (fun char_code bool_code acc->acc+if char_code<>(-1) then begin arr.(char_code)*(List.length bool_code) end else 0) (arr |> build_huffman_tree |> tree_to_dict) 0) (8*String.length str);;

let ()=ratio (String.init 10000 (fun i->char_of_int (1+Random.int 255)));;

let encode_char obitfile d k=
  List.iter (fun b->output_bit obitfile b) (Hashtbl.find d k);;

let encode_text obitfile d str=
  String.iter (fun char->encode_char obitfile d (int_of_char char)) str;;

let fill l=(List.init (List.length l-9) (fun _->false))@l

let rec pow2 =function
| 0 -> 1
| k -> 2*pow2 (k-1)

let rec get_bit_list k=
  if k=(-1) then get_bit_list 256 else
  let rec iter l =function
  | 0 -> if l=1 then [true] else [false]
  | k -> if l<pow2 k then false::iter l (k-1) else true::iter (l-pow2 k) (k-1)
  in iter k 8;;

let encode_leaf obitfile k =
  output_bit obitfile false; List.iter (output_bit obitfile) (get_bit_list k);;

let encode_tree obitfile tr=
  let rec iter_tree=function
  | Leaf (k,_) -> encode_leaf obitfile k
  | Node (_,st1,st2) ->  iter_tree st1;iter_tree st2;output_bit obitfile true
in iter_tree tr;;

let compress str filename=
  let tr=str |> char_counts |> build_huffman_tree and obitfile=open_bit_out filename in
  encode_tree obitfile tr;
  let d=tr |> tree_to_dict in
  output_bit obitfile true ; encode_text obitfile d str; List.iter (output_bit obitfile) (Hashtbl.find d (-1));close_bit_out obitfile;;


let int_of_b_list l=
List.fold_left (fun hr v->2*(hr+if v then 1 else 0)) 0 l/2;;

let decode_tree ibitfile =
  let stack=ref [] in
  let rec iter ()=if read_bit ibitfile |> not then begin
    stack:=Leaf ((List.init 9 (fun _->read_bit ibitfile) |> int_of_b_list),0)::(!stack);iter ()
  end else match !stack with
  | [] -> failwith "Empty tree, nothing has been encoded."
  | [tree]-> tree
  | tr1::tr2::tl-> stack:=Node (0,tr2,tr1)::tl; iter ()
in iter ();;

exception CorruptFile;;
let rec visit_tree b_list tr=match b_list,tr with
| [],tr -> tr
| _,Leaf (a,b) -> Printf.printf "%d %d" a b; raise CorruptFile
| h::t,Node (_,st1,st2) -> if h then visit_tree t st2 else visit_tree t st1;;

let print_bool_l l=List.iter (fun v->Printf.printf "%d" (if v then 1 else 0)) l;Printf.printf "\n";;

let decode_text ibitfile tree=
  Printf.printf "%d\n" ibitfile.pos;
  let rec acc=ref [] and count=ref 0 and 
  iter lst=
    let n_l=lst@[read_bit ibitfile] in
    match visit_tree n_l tree with
    | Leaf (256,_) | Leaf (-1,_) -> () (* à priori, c'est le 256 qui devrait engendrer l'arrêt. Mais comme les -1 et 256 sont des éléments deux à deux ambiguës, je ne prends pas de risques. *)
    | Leaf (ch_c,_) -> acc:=(char_of_int ch_c |> String.make 1)::(!acc); incr count;iter []
    | Node _ -> iter n_l
in iter [];String.concat "" (List.rev (!acc));;

let deflate inputfilename=
  let ibitfile=(inputfilename |> open_bit_in) in
  let tree=decode_tree ibitfile in
  Printf.printf "%d\n" ibitfile.pos;
  (*for j=0 to 80 do
    read_bit ibitfile;
  done;*)
  Printf.printf "%d\n" ibitfile.pos;
  let res=decode_text ibitfile tree in (*close_bit_in ibitfile;*) 
  res;; (* La partie mise en commentaire n'existe pas. Elle est inutile, car le fichier est fermé dès que l'ensemble du fichier a été bufferisé*)

let print_bits l=List.iter (fun v->if v then print_int 1 else print_int 0) l;Printf.printf"\n";;