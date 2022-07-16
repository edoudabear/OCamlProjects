exception SDError of string

let isEmpty lst = match lst with
    | [] -> true
    | _ -> false

let rec last lst = match lst with
    | [] -> None
    | [ x ] -> Some x
    | _ :: t -> last t

let rec last2 lst = match lst with
    | [] | [_] -> None
    | [ x ; y ] -> Some (x , y)
    | _ :: t -> last2 t

let rec nth lst count = match lst with
    | [] -> None
    | h :: t -> if count=0 then Some h else nth (t) (count-1)

let rec length lst = match lst with
    | [] -> 0
    |  _ :: t -> 1 + length t

let rev lst =
    let rec aux res = function
       | [] -> res
       | h :: t -> aux (h :: res) t
    in aux [] lst

let rec reverse = function (* not optimal  (because of list append)*)
| [] -> []
| h :: t -> (reverse t) @ [h];;

let palindrome lst = 
    lst = rev lst

let print_bool booval = match booval with
    | true -> print_string "true\n"
    | _ -> print_string "false\n"

let rec contain what lst = match lst with
    | [] -> false
    | h::t -> if h=what then true else contain (what) (t)

let compress lst =
    let rec aux lst = match lst with
        | [] -> lst
        | h :: t -> if contain h t then aux t else h :: aux t
    in aux lst

let rec remove_at index = function
    | [] -> []
    | h::t -> if index=0 then remove_at (-1) t else h :: remove_at (index-1) t


let rec insert_at value index lst = 
    let rec aux ind = function
        | [] -> if ind>0 then raise (SDError "Index out of range") else if ind=0 then [value] else [] 
        | h :: t -> if ind=0 then value :: h :: aux (ind-1) t else h :: aux (ind-1) t
    in aux index lst

let rec range min max step = match max with
    | max -> if max>min then min :: range (min+step) (max) step else if max=min then [min] else if min<max+step then [] else raise (SDError "Max is bigger than min..")

let out=[1;2;1] |> palindrome |> print_bool ;