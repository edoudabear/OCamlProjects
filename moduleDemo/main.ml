module Stack = struct
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
