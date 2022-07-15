(** for i = 1 to 10 do print_int(i); print_endline(". Etappe");  done *)

let sum x y = x +. y;;
print_endline "Enter number one :";;
let x= read_float() ;;
print_endline "Enter number two :";;
let y= read_float() ;;

print_endline (
    string_of_float (
        sum x y
    )
);;

type project = {
    name : string;
    projectType : string;
    creationDate : int; (* Date of project developpement begin *)
}

let diskloudPages = {
    name = "Diskloud Pages";
    projectType = "Web Page Creation Platform";
    creationDate = 2021;
}