# let rec insert a =function
| [] -> [a]
| h::t when h>a ->a::h::t
| h::t -> h::insert a t;;
val insert : 'a -> 'a list -> 'a list = <fun>
# let rec tri_inser=function
| [] -> []
| h::t -> insert h (tri_insert t);;
Error: Unbound value tri_insert
Hint: Did you mean tri_inser?
# let rec tri_inser=function
| [] -> []
| h::t -> insert h (tri_inser t);;
val tri_inser : 'a list -> 'a list = <fun>
# tri [5;4;3;2;1];;
Error: Unbound value tri
# tri_inser [5;4;3;2;1];;
- : int list = [1; 2; 3; 4; 5]
# tri_inser [5;2;3;4;1];;
- : int list = [1; 2; 3; 4; 5]
# let rec tri_bulle = function
| [] -> []
| l -> let a,b=min_et_reste l in a::tri_bulle b and
min_et_reste = function
| [a] -> (a,[])
| l -> let rec min_l = function
       | [a] -> a
       | h::t -> min h (min_l t) and del_f v = function
          | h::t when v=h -> t
          | h::t -> h::del_f v t
          | [] -> []
       in let m=min_l l in (m,del_f m l);;
val tri_bulle : 'a list -> 'a list = <fun>
val min_et_reste : 'a list -> 'a * 'a list = <fun>
# tri_bulle [5;4;3;2;1];;
- : int list = [1; 2; 3; 4; 5]
# tri_bulle [5;4;2;3;1];;
- : int list = [1; 2; 3; 4; 5]
# tri_bulle [5;4;2;3;42];;
- : int list = [2; 3; 4; 5; 42]
# #help
;;
# #utop_help
;;
