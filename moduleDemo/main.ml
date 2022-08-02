module type STACK =
	sig
		val empty : 'a list
		exception Empty
		val push : 'a -> 'a list -> 'a list
		val peek : 'a list -> 'a
		val pop : 'a list -> 'a list
	end;;

module Stack : STACK = struct
	let empty = [];;
	exception Empty ;;
	let push a lst = a::lst;;
	let peek = function
	| [] -> raise Empty
	| a :: _ -> a;;
	let pop = function
	| [] -> raise Empty
	| _ :: t -> t;;
end;;

let _ = print_int (
	Stack.(empty |> push 2 |> push 3 |> pop |> peek)
); print_char '\n';;
