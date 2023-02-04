
(*
   @param adore liste des noms du film adoré de chaque personne
   @param deteste liste des noms du film détesté de chaque personne
*)

let rec listContains el = function
| [] -> false
| [a] -> if a <> el then false else true
| h::t -> if h <> el then true else listContains el t;;

let nombreFilms adore deteste =
  let rec aux acc deteste = function
  | [] -> acc
  | h::t -> if listContains h deteste then
                aux acc deteste t
            else
                aux (acc+1) deteste t
in aux 0 adore deteste;;

let () =
  (*let adore = List.init 6 (fun _ -> read_line ()) in
  let deteste = List.init 6 (fun _ -> read_line ()) in
  print_int (nombreFilms adore deteste);*)
  print_int (nombreFilms ["Haha","Hehe","Hihi","Hoho","Huhu","Wouhou"] ["Hyhy","Hrhr","Hoho","Huhu","Huhu","Hyhy"])
;;
