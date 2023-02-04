

let rec  opti n k p lst =
 let rec aux debut = function
| [] | [_] | [_,_] | [_,_,_] -> 0
| n1::h2::h3::n2::t -> 
        if n1-n2 < seuil -> max (aux debut 
        else max 0 (aux h2::h3::n2::t)
 and seuil= p |> float_of_int |> sqrt |> int_of_float
 in aux 
 
let sort lst =
   let rec aux acc = function
   | [] -> acc
   | h::t -> let rec maxFinder = function
            | (a,h::t) when a<h -> maxFinder (h,t)
            | (a,h::t) -> maxFinder (a,t)
            | (a,[]) -> a 
            and del a = function
            | h::t when h=a -> t
            | [] -> []
            | h::t -> h::(del a t)
            in aux ((maxFinder (h,t))::acc) (del (maxFinder (h,t)) (h::t))
    in aux [] lst;;