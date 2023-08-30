let rec gray = function
| 0 -> [] 
| 1 -> ["0";"1"]
| n when n>1 -> let lst=gray(n-1) in (List.map (fun h -> "0" ^ h) lst) @ List.rev ( List.map (fun h->"1" ^ h) lst)
| _ -> failwith "NO NEGATIVE INTEGERS PLEASE.." ;;
