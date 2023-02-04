let rec print_loop str = print_string str;print_string "\n"; print_loop str;;

let main = let a = Domain.spawn (fun _ -> print_loop "Edouard Aubert")
           and b = Domain.spawn (fun _ -> print_loop "Diskloud") in ();;

let () = main;;
