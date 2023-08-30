let bar l=
  let rec aux1 count lst= match count with
  | -1 -> lst
  | c -> aux1 (count-1) (aux2 count 0 lst)
  and aux2 k sm lst = match k,lst with
  | _,[]->[]
  | 0,h::t -> (sm+h)::aux2 0 (sm+h) t
  | a,h::t -> h::aux2 (a-1) 0 t
in aux1 (List.length l - 2) l;;

let rec diff = function
| [] | [_] -> []
| h1::h2::t -> (h2-h1)::diff (h2::t);;

let rec gen_a a_0 = function
| [] -> [a_0]
| h::t -> (a_0)::gen_a (a_0+h) t;;

let rec foo = function (* a_j -> b_j *)
| [] -> []
| [x] -> [x]
| h::t -> h::foo (diff (h::t));;

(* b_0 = a_0
   b_1 = a_1-a_0
   b_2 = a_2-a_1-b_1 *)