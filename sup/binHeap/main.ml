type 'a binHeap = 
| Node of int * 'a binHeap * 'a binHeap
| Leaf
;;

let rec insert value = function
| Leaf  -> Node (value,Leaf,Leaf)
| Node (value1,value2,value3) when value<value1 -> Node (value,insert value1 value2,value3)
| Node (value1,Node (value2,child1,child2),value3) when value<value2 -> Node (value1,insert value (Node (value2,child1,child2)), value3)
| Node (value1,value2,value3) -> Node (value1,value2,insert value value3);;

let poke = function
| Leaf -> failwith "Nothing to poke"
| Node (value,value2,value3) -> let rec aux = function
                                | Leaf -> Leaf
                                | Node (_, Leaf, Leaf) -> Leaf
                                | Node (_,Node (value2,child1,child2),Leaf) -> Node (value2,child1,child2)
                                | Node (_,Leaf,Node (value3,child1,child2)) -> Node (value3,child1,child2)
                                | Node (_,Node (value2,child1,child2),Node (value3,child3,child4)) when value2<value3  -> Node (value2,(aux (Node (value2,child1,child2))),Node (value3,child3,child4))
                                | Node (_,Node (value2,child1,child2),Node (value3,child3,child4)) -> Node (value3,Node (value2,child1,child2),aux (Node (value3,child3,child4)))
                                in (value,aux ((Node (value,value2,value3))));;