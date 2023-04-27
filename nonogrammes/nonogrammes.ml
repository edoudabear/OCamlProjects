

(* Affichage d'une grille *)

let rec sum=function
| [] -> 0
| h::t -> h+sum t;;

let rec next n=function
| [] -> None
| h::t when h=n -> begin match next n t with | Some v -> Some (0::v) | None -> None end
| h::t -> Some ((h+1)::t);; 

let list_sols s n=
  let rec iter acc=function
  | None -> acc
  | Some v -> if sum v=s then iter (v::acc) (next n v) else iter acc (next n v)
  in iter [] (Some (List.init n (fun _->0)));;

let white_blocks i_l n=
  let rec filter = function
  | [] -> []
  | t::q -> begin let rec test=function
            | [v] -> t::filter q
            | 0::q2 -> filter q
            | v::q2 -> test q2
            | _ -> filter q
            in test (List.tl t)
            end
  in filter (list_sols (n-sum i_l) (List.length i_l+1));;

let build white black=
  let white=ref white and black=ref black and isWhite=ref true and n=sum white+sum black in let res=Array.make n 0 in
  for i=0 to n-1 do
    if (!isWhite) then
      let v=List.hd (!white) in
      if v=1 then
        (isWhite:=false;
        white:=List.tl (!white);
        )
      else if v=0 then
        (isWhite:=false;
        white:=List.tl (!white);
        black:=List.hd (!black)-1::(List.tl (!black));
        res.(i)<-1)
      else
        white:=(v-1)::(List.tl (!white))
    else 
      let v=List.hd (!black) in
      if v=1 then
        (isWhite:=true;
        black:=List.tl (!black))
      else
        black:=(v-1)::(List.tl (!black));
      res.(i)<-1
    done;
    res;;

let solutions l_i n=
  let rec convert= function
  | []->[]
  | h::t ->(build h l_i)::convert t
  in convert (white_blocks l_i n);;

let result l_i n=
  let rec output_test i v=function
  | [] -> v
  | h::t when h.(i)=v -> output_test i v t
  | h::t -> -1
  in let sols=solutions l_i n in if sols=[] then [||] else let f=List.hd sols in Array.mapi (fun i v->output_test i v sols) f
;;

exception Faux;;

let is_compatible arr1 arr2=
  let n=Array.length arr1 in
  try
    for i=0 to n-1 do
      if arr2.(i)<>(-1) && arr1.(i)<>arr2.(i) then raise Faux;
    done ;
    true
    with Faux -> false;;

let result_with_data  l_i connus=
  let n=Array.length connus in
  let rec filter =function
  | [] -> []
  | h::t -> if is_compatible h connus then h::filter t else filter t
  and output_test i v=function
  | [] -> v
  | h::t when h.(i)=v -> output_test i v t
  | h::t -> connus.(i)
in let sols=solutions l_i n |> filter in if sols=[] then connus else Array.mapi (fun i _-> output_test i (List.hd sols).(i) sols) connus;;


let draw arr =
  let nbl = Array.length arr
  and nbc = Array.length arr.(0) in
  let draw_line () =
    print_string "+";
    print_string (String.make nbc '-');
    print_string "+\n" in
  draw_line ();
  for i = 0 to nbl - 1 do
    print_string "|"; 
    Array.iter
      (fun i -> print_char (match i with 0 -> ' ' | 1 -> 'X' | _ -> '?')) arr.(i);
    print_string "|\n"; 
  done;
  draw_line ()

(* Extraction et insertion d'une ligne/colonne *)

let get_line i arr =
  let n = Array.length arr.(i) in
  let res = Array.make n 0 in
    for j = 0 to n-1 do res.(j) <- arr.(i).(j) done;
    res

let get_col j arr =
  let n = Array.length arr in
  let res = Array.make n 0 in
    for i = 0 to n-1 do res.(i) <- arr.(i).(j) done;
    res

let set_line i line arr =
  for j = 0 to (Array.length line - 1) do
    arr.(i).(j) <- line.(j)
  done

let set_col j col arr =
  for i = 0 to (Array.length col - 1) do
    arr.(i).(j) <- col.(i)
  done

(* Création d'une grille vide *)

let init_grid (clue_l, clue_c) =
  Array.make_matrix (List.length clue_l) (List.length clue_c) (-1)

(* Pour la résolution de grilles difficiles *)

exception Impossible

let copy_grid arr = Array.map Array.copy arr

(* Puzzles *)

let puzzle1 = ( [ [1;2]; [2]; [1]; [1]; [2]; [2;4]; [2;6]; [8]; [1;1];
                  [2;2] ],
                [ [2]; [3]; [1]; [2;1]; [5]; [4]; [1;4;1]; [1;5]; [2;2];
                  [2;1] ] );;

let puzzle2 = ( [ [5]; [9]; [2;1;1;1;2]; [1;1;1;1;1]; [11]; [1]; [1]; 
                  [1]; [1]; [1;1]; [3] ],
                [ [3]; [2;1]; [1;2]; [3;1]; [2;1]; [11]; [2;1;1];
                  [3;1;2]; [1;2]; [2;1]; [3] ] );;

let puzzle3 = ( [ [1]; [1]; [2;2]; [1;5]; [8]; [6]; [2;3] ],
                [ [2]; [1;1;1]; [7]; [3]; [5]; [5]; [4]; [2] ] );;

let puzzle4 = ( [ [1;1]; [3]; [3]; [4]; [6]; [5]; [1;1;3]; [6] ],
                [ [3;1]; [4;1]; [8]; [3;1]; [5]; [4]; [4] ] );;

let puzzle5 = ( [ [3]; [1;1]; [1;1;1]; [2;1]; [1;5]; [1;1;1];
                  [1;3;1]; [2;2]; [7]; [1;1] ],
                [ [1]; [7]; [1;2]; [1;1;1]; [1;2]; [4;1;1];
                  [1;1;2]; [3;1]; [1;2]; [4] ] );;

let puzzle6 = ( [ [5]; [1;5]; [3;1]; [2;2]; [3;3]; [5;4]; [2;4;2]; 
                  [1;1;2]; [1;4;1]; [4;7]; [4;2;2]; [2;2;2;1]; 
                  [1;1;1;1]; [1;1;1;1]; [1;1;1] ],
                [ [1;3]; [2;2;1]; [1;5]; [3;2]; [1;4]; [1;1;3;3]; 
                  [1;1;1;1]; [1;2;2]; [1;1;1;3]; [1;1;1;3;1]; 
                  [3;2;2]; [5;1]; [1;7]; [2;4;2;1]; [1;3] ] );;

let puzzle7 = ( [ [2]; [1;2]; [1;1]; [2]; [1]; [3]; [3]; [2;2]; 
                  [2;1]; [2;2;1]; [2;3]; [2;2] ],
                [ [2;1]; [1;3]; [2;4]; [3;4]; [4]; [3]; [3]; [3]; 
                  [2]; [2] ] );;

let puzzle8 = ( [ [2]; [2]; [2]; [2]; [1] ],
                [ [2]; [2]; [2]; [2]; [1] ] );;

let puzzle9 = ( [ [0]; [2;3]; [1;1;1]; [1;1;4]; [8;1]; [5;2;2]; 
                  [1;2;2;1]; [2;1;1;1;1;1]; [1;1;5]; [1;1;1;1;3]; 
                  [4;2;2;1;1]; [3;2;2;1]; [1;2;2;3]; [2;4;1]; 
                  [3;1;2;2;1]; [1;1;3;2]; [2;1;2]; [6]; [1;1;2]; 
                  [2;2;1]; [2;4]; [2]; [1]; [2]; [1] ],
                [ [2]; [1]; [1]; [1]; [2]; [2]; [3;1]; [4;1;1;1]; 
                  [3;1;2;1]; [1;4;2]; [7;1;1]; [5;2;2;1]; 
                  [1;2;2;1;5]; [1;1;1;1;5;2]; [1;1;1;2;1;1]; 
                  [1;1;1;1;1;1]; [1;1;1;2;2]; [5;2;1;2]; [3;2;1]; 
                  [1;6;1]; [1;2;2]; [1;2;1]; [3;1;3]; [1;1]; [2] ] )
                  
;;

(* Fin du préambule *)

let solve_lines puzzle arr=
  let lineData=fst puzzle in
  let rec iter_line k=function
  | [] -> ()
  | i_l::t-> let line=get_line k arr in
      set_line k (result_with_data i_l line) arr; iter_line (k+1) t
in iter_line 0 lineData;;

let solve_cols puzzle arr=
  let colData=fst puzzle in
  let rec iter_col k=function
  | [] -> ()
  | i_c::t-> let col=get_col k arr in
            set_col k (result_with_data i_c col) arr; iter_col (k+1) t
in iter_col 0 colData;;

let count arr= Array.fold_left (fun h v->h+Array.fold_left (fun h2 v2-> h2+if v2=(-1) then 1 else 0) 0 v) 0 arr;;

let solve_1 puzzle play=
  while count play>0 do
    solve_lines puzzle play;
    solve_cols puzzle play;
  done;;

let grid=init_grid puzzle1;;

solve_1 puzzle1 grid;;

draw grid;;