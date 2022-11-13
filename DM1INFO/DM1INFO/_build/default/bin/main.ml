open OUnit2;;
exception Oops of string;;

let print_arr lst = 
  let rec aux = function
  | [] -> print_endline "]";
  | [a] -> print_int a; print_endline "]";
  | h::t -> print_int h;print_char ','; aux t;
in print_char '[' ; aux lst;;

let rec count = function
| [] -> 0;
| h::t -> h + count t;;

let rec isFinished=function
| [] -> true
|  h::t -> (h=0) && (isFinished t);;

let isValid lst k =
  (List.nth lst k)=(k+1);;

let next lst k=
  let rec aux lst kA acc =
    match lst with
    | [] -> List.rev acc
    | h::t -> 
      if kA>0 then (aux (t) (kA-1) ((h+1)::acc))
      else if kA=0 then (aux (t) (-1) (0::acc))
      else (aux (t) (0) (h::acc));
in aux lst k [];; 

let rec possible lst = if isFinished lst then true else
  let rec aux lst lenLst = match lenLst with
  | -1 -> false;
  | a -> if isValid lst a then (possible (next lst a) || aux lst (a-1)) else aux lst (a-1)
  in aux lst (List.length lst-1)
;;

(* EN PANNE let solution lst = if not (possible lst) then [-1] else
  let rec aux lst lenLst acc=if isFinished lst then List.rev acc else match lenLst with
  | -1 -> raise (Oops "Unexpected end of iteration");
  | a -> if (isValid lst a) && (possible (next lst a)) then (aux lst lenLst (a::acc)) else aux lst (a-1) acc;
in aux lst (List.length lst -1) []
;; *)

let set1 = function
| _::t -> 1::t
| _ -> []
;;

let problem n=
  if n<0 then raise (Oops "Invalid argument (n<0)") else
  let rec aux k acc=
        if k=n then acc
        else if k mod 2=0 then
            let rec aux2 lst acc count = match lst with
            | [] -> List.rev acc;
            | h::t -> if count<>0 && h=0 then aux2 t (count::acc) 0
                      else if count<>0 then aux2 t ((h-1)::acc) (count+1)
                      else aux2 t (h::acc) 0
            in aux (n+1) (aux2 acc [] 1)
         else
            aux (n+1) (set1 acc)
  in aux 1 (List.init n (fun _->0));; 
let test1 test_ctxt value = assert_equal value (test_ctxt [1;2;3;4;5;0;6;0;0]);;
let test2 test_ctxt value = assert_bool "TEST2 Failed" (value=test_ctxt [0;0;0;1;2;3;4;5;6]);;

let test3 test_ctxt value = assert_bool "TEST3 Failed" (value=test_ctxt);;

let test4 test_ctxt value = assert_equal value (test_ctxt);;

(* Name the test cases and group them together *)

(* let print_bool = function
| true -> print_endline "true";
| _ -> print_endline "false"
;; *)

let () =
  print_arr [1;2;3;4;5;6];
  test1 count (1+2+3+4+5+6);
  test2 isFinished false;
  test3 (isValid [0;3;5;8;2] 1) false;
  test4 (next [1;2;3;4;5] 3) [2;3;4;0;5];
  test3 (possible [1;2;0;2;4;6]) true;
  (* test4 (solution [1;2;0;2;4;6]) [ 0; 1; 0; 5; 0; 4; 0; 1; 0; 3; 0; 2; 0; 1; 0] !! COMPLEXITE TROP IMPORTANTE !! *)
  print_arr (problem 4)
;;