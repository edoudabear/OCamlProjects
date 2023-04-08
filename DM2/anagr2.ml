(* 
Edouard Aubert 2023
*)

type cards= Letter of char*int | Trump_card of int ;;

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


(* Cette fonction est appelée avant de chercher des mots valides. Elle vérifie déjà si 
   la requête a des chances de retourner une réponse quelconque, auquel cas on démarre l'itération*)
let control str = if String.length str>15 then false else String.fold_left (fun prev h -> prev && ((Char.code h)>=97 && (Char.code h)<=122 || h='?')) true str;;

(* Ce tableau généré à partir de la liste contient l'ensemble des éléments de la liste
   words_list.*)
let word_tbl=
  let arr=Array.make (dico_size) ("",0) in
  let rec copy i =function
  | [] -> ()
  | h::t ->  arr.(i) <- (h,String.length h); copy (i+1) t
in copy 0 words_list; arr;;
let word_data=
let nodes=Array.make (15*dico_size) (0 |> char_of_int) and
    links=Array.make (15*dico_size) (0,0) and
    adj=Array.make (15*dico_size) (0,0,0) and
    last_link_ptr=ref (-1) in
for i=0 to 15 do begin
  let last_node_ptr=ref (-1) in
  Printf.printf "i : %d\n" i;
  for j=0 to dico_size-1 do
    let str=fst word_tbl.(j) in
    if String.length str>i && (!last_node_ptr=(-1) || nodes.(!last_node_ptr)<>(str.[i])) then begin
      incr (last_node_ptr);
      nodes.(!last_node_ptr)<-str.[i];
    end else begin () end;
    if i>0 && String.length str>i && (!last_link_ptr=(-1) || links.(!last_link_ptr)<>(str.[i-1],str.[i])) then begin 
      incr last_link_ptr;
      links.(!last_link_ptr)<-(str.[i-1],str.[i])
    end
  done;
end done; (nodes,links,adj);;

let c3_1 =function | (a,_,_) -> a ;;
let c3_2 =function | (_,b,_) -> b ;;
let c3_3 =function | (_,_,c) -> c;; 

let () = Array.iter (fun (a,b) -> Printf.printf "%c,%c" a b;) (c3_2 word_data);;

(* Cette fonction génère le tableau représentant la répartitions des caractères entrés par l'utilisateur (cf. compte-rendu) *)
let generate_cards str =
  let cards=Array.make (27) (0) in
  let rec aux = function
  | [] -> cards
  | h::t when h='?' -> cards.(26) <- cards.(26)+1; aux t;
  | h::t -> cards.(Char.code h-97) <- cards.(Char.code h-97)+1; aux t;
  in aux (str |> String.to_seq |> List.of_seq)
;;

(* Fonction mettant à jour le contenu du tableau de répartition en fonction du caractère donné
   NB : le true,false retourné en plus permet de savoir si le caractère donné est compatible avec le tableau*)
let rec update_cards c cards=if cards.(Char.code c-97)=0 then (* Si l'utilisateur n'a pas demandé autant de caractères *)
                                    if cards.(26)=0 then begin (* Est ce qu'il nous reste des jokers ? *)
                                      cards,false
                                    end else begin
                                    cards.(26) <- cards.(26)-1;cards,true
                                    end else begin
                                      cards.(Char.code c-97)<-cards.(Char.code c-97)-1;cards,true
                                    end
;;

exception Exit_loop;;

(* Cette fonction vérifie un mot en itérant sur ses caractères pour mettre à jour le "jeu de cartes", tant que celui-ci est compatible avec le mot. *)
let check_word cards word=
  let cards_state=ref (cards, true) in
  try
    for i=0 to String.length word - 1 do
      cards_state:=(update_cards (word.[i]) (fst (!cards_state)));
      if snd (!cards_state)=false then raise Exit_loop else ()
    done; (true)= snd (!cards_state)
  with
  | Exit_loop -> false
;;

(*Permet simplement de copier le contenu d'un tableau de 27 cases vers un autre *)
let reset_cards dest origin=
for i=0 to 26 do
  dest.(i)<-origin.(i)
done;;

(* Itération du test de validité d'un mot avec la requête sur chaque mot du dictionnaire *)
let check1by1 str =
  let l=String.length str and
  cards=generate_cards str in
  let cardsCopy=Array.make (27) (0) in (* On génère deux tableaux au début, ce qui nous permet d'éviter de faire un allocation à chaque itération. *)
  let rec aux=function
  | n when n=dico_size -> ()
  | n -> reset_cards cardsCopy cards ; begin if (snd word_tbl.(n))=l && check_word cardsCopy (fst word_tbl.(n)) then  Printf.printf "%s\n" (fst word_tbl.(n)) else () end; aux (n+1)
in aux 0 ;;

let search str = if control str then check1by1 str else ();;

(* Boucle principale*)
let () =
  let rec loop () =
    let req = print_string "Lettres : "; read_line () in
      if req = "!" then () else (search req; loop ())
    in loop ()
;;