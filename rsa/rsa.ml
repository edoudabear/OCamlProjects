(* RSA Project *)
exception RSAError of string;;
open Printf;;

let crypt (y : int) (b : int) (n : int) = (* crypt=y^b[n]*)
    let rec cryptInside (y : int) (b : int) (n : int) (modVal : int) =
        match b with
        | 0 -> 0
        | 1 ->  modVal mod n
        | _ -> cryptInside y (b-1) n ((modVal*y) mod n)
    in cryptInside y b n y
;;


let cryptMain n a x = print_int (crypt x a n);;
let n=23*5;;
let m=22*4;;

(* 3b-88c=1 
   a=3
   b=59
   c=2
*)

let pgcd a b =
    let rec pgcdLoop x y testVal =
        if x mod testVal=0 && y mod testVal=0 then testVal else if testVal>1 then pgcdLoop x y (testVal -1) else 1
    in pgcdLoop a b (min a b);;

let algoEuclide a b =
    (* data representation : with (x,y,z,a) -> x=ay+z *)
    let rec inside x y = function
    | 1 -> (x,y,1,(x-(x mod y))/y) :: []
    | a -> (x,y,a,(x-(x mod y))/y) :: inside (y) (x mod y) (y mod (x mod y))
    in inside (max a b) (min a b) ((max a b) mod (min a b));;

let elem number = function
    | (a,b,c,d) -> if number=1 then a 
    else if number=2 then b
    else if number=3 then c
    else if number=4 then d
    else raise (RSAError "Invalid index number");;

let rec remonteeEuclide data = 
    let rec inside equality = function
    | [] -> raise (RSAError "Invalid output")
    (* a=kb+c*)
    | [(a,b,c,k)] -> if equality=(0,0,0,0) then
        (a,b,1,-k)
        else
        ((elem 1 equality),a,-k,1+k*(elem 4 equality))
    | h :: t ->
        if h::t=data then (inside h t) else (*First equality case : nothing to change to output value*)
        match (inside h t) with 
        (* with (a,b,ka,kb) -> 1=aka+bkb and equality (z,x,y,k) -> z=kx+y *)
        | (a,b,ka,kb) -> ((elem 1 equality),a,kb,ka-(kb*(elem 4 equality)))
    in inside (0,0,0,0) data;;

let diophantienne a b =
    (* Function solving equation of type ax+by=1 where (a,x,b,y) belong to (n*n*n*n)
       OUTPUT type : (a,b,x,y) where x and y are specific solutions *)
    if pgcd a b != 1 then
        raise (RSAError "Invalid input data :/ Both number have common dividers" )
    else if a<=0 || b<=0 then
        raise (RSAError "Arguments must be natural numbers different from 0 (strictly positive)")
    else
        match remonteeEuclide(algoEuclide a b) with
        | (a,b,c,d) -> (a,b,c,d)
    ;;

let rec decompose a =
    if (a mod (2))=0 then 2::(decompose (a/2))
    else let rec inside a d =
        if a=1 then [] else
        if d=1 then a::[]
        else if (a mod d)=0 then d::(inside (a/d) (d/2))
        else inside a (d-1)
    in inside a (a/2)
;;

let rec isPremier a = function
    | [x] -> (pgcd a x)=1
    | h::t -> (pgcd a h)=1 && (isPremier a t)
    | _ -> raise (RSAError "Impossible error")
;;


let rec reverse = function
| [] -> []
| h::t -> (reverse t) @ [h]
;;

(* Very bad performance *)
let premiers x =
    if x<1 then raise (RSAError "Invalid range value") else
    let rec test = function
    | 1 -> []
    | 2 -> [2]
    | a -> if (List.length (decompose a))=1 then a::test (a-1) else test (a-1)
    in reverse (test (x))
;;

let printIntList x = print_char '['; 
    let rec printIntInside = function
    | [] -> print_char ']';
    | h::t -> print_int h;if t!=[] then print_string " ; ";printIntInside t;
in printIntInside x ; print_char '\n';;

let premiersMain x = printIntList (premiers x);;

(* let () = List.iter (printf "%d ") (premiers 25924) *)
(* let () = printf "\n" *)

let fst = function
| (_,_,a,_) -> a
;;

let scnd=function
| (_,_,_,b) -> b
;;

let diophantienneMain a b=
    let mainIn a b res = printf ("One particular solution (x;y) of %dx+%dy=1 is : (%d;%d)\n") (max (a) (b)) (min (a) (b)) (fst (res)) (scnd (res))
    in mainIn a b (diophantienne (a) (b));
;;

(*let rec diophantienne a b = if (pgcd a b) != 1 then None else algEuclide a b*)
