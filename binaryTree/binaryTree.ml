type 'a tree =
   | Leaf
   | Node of 'a * 'a tree * 'a tree

let t = Node(4,
        Node (5,
            Leaf,
            Leaf
        ),Node (3,
            Leaf,
            Node (
                2,
                Leaf,
                Leaf
                )
            )
        )

let rec size = function
    | Leaf -> 0
    | Node (_, left, right) -> 1 + size left + size right

let rec sumVal = function
    | Leaf -> 0
    | Node (valu, left, right) -> valu + (sumVal (left) + sumVal (right))

let rec exists searchVal = function
    | Leaf -> false
    | Node (valu,left,right) -> valu=searchVal || exists searchVal left || exists searchVal right

let print_bool = function
    | true -> print_string "true"
    | false -> print_string "false"

let () = print_int (size t)
let () = print_char ('\n')
let () = print_string("Sumval : ")
let () = print_int (sumVal t)
let () = print_char ('\n')
let () = print_bool (exists 5 t)
let () = print_char ('\n')
let () = print_bool (exists (-1) t)
let () = print_char ('\n')