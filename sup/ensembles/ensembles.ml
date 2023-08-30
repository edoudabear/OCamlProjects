type set=
  | Leaf of int list
  | Union of set*set
  | Inter of set*set
  | Minus of set*set;;

let rec set_mem a =function
| Leaf s->List.mem a s
| Union (s1,s2) -> set_mem a s1 || set_mem a s2
| Inter (s1,s2) -> set_mem a s1 && set_mem a s2
| Minus (s1,s2) -> set_mem a s1 && not (set_mem a s2);;

let list_of_set s=
  let rec removeDups =function
  | h::t -> if List.mem h t then removeDups t else h::removeDups t
  | [] -> []
  and listize =function
  | Leaf s-> s
  | Union (a,b) | Inter (a,b) | Minus (a,b) -> (listize a)@(listize b)
in s |> listize |> removeDups |> List.filter (fun a-> set_mem a s);;

let ens= (* Si tout va bien, list_of_set devrait retourner [1;3;4]*)
  Minus (
    Union (Leaf [1;3;4],Leaf [2;5])
  , Inter (Leaf [2;4;5],Leaf [1;2;3;5])
  )
