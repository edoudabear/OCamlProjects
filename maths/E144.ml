let range n =
  let rec aux lst i =
    if i<1 then
      lst
    else
      aux (i::lst) (i-1)
  in aux [] n 
;;

let rec (>>) value = function
| [] -> false
| h::t -> if h=value then true else value >> t

let delCopies lst =
  let rec aux res = function
  | [] -> res
  | h::t -> if h >> t then aux res t else aux (h::res) t
in aux [] lst;;

let u n = (range n) |> List.map (fun h -> n/h) |> delCopies |> List.length;;

(* let _ = print_int(u (print_endline "Enter series index :"; read_int ()) ); print_string "\n\n";; *)

let _ =
  let rec aux i n= if i>n then exit 0 else Printf.printf "%d\t\t%d\n" (i) (u i); aux (i+1) n 
in aux (1) (print_endline "Enter series index range (1-'your answer') :" ; read_int ())