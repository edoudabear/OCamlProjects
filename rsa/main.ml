(*ocamlfind ocamlopt -g rsa.ml main.ml -o main*)
let usage_msg = "append [-diophantienne]  [-premiers] [-crypt] ... ";;
let input_files = ref [];;
let anon_fun filename =
  input_files := filename :: !input_files;;
let diop_mode = ref false
and prem_mode = ref false
and crypt_mode = ref false;;
let speclist =
  [("-diophantienne", Arg.Set diop_mode, "Chooses to call diophantienne mode (gives a specific solution of a user-defined diophantienne equation)");
  ("-premiers",Arg.Set prem_mode,"Chooses to call premiers mode (lists all premiers numbers from 2 to a user-defined value)");
  ("-crypt",Arg.Set crypt_mode,"Allows to encrypt and decrypt data with two keys")]
let () =
  Arg.parse speclist anon_fun usage_msg;;
let main = if !diop_mode then 
    let diophantienne_loop = print_endline "diophantienne mode selected"; ref false in
    while not !diophantienne_loop do
        let str = Rsa.diophantienneMain (Printf.printf "Enter the second coefficient : "; read_int ()) (Printf.printf "Diophantienne (ax+by=1)\nEnter the first coefficient : "; read_int ()) ; print_string "Have another equation ? (y/n) "; read_line () in
        if str.[0] = 'n' then diophantienne_loop := true
    done;
  else if !prem_mode then
    let premiers_loop = print_endline "premiers mode selected"; ref false in
    while not !premiers_loop do
        let str = Rsa.premiersMain (Printf.printf "Enter the maximal int of the range (must be greater than 2) : "; read_int ()); print_string "Have another range ? (y/n) "; read_line () in
        if str.[0] = 'n' then premiers_loop := true
    done;
  else if !crypt_mode then
    let crypt_loop = print_endline "encrypt/decrypt mode selected"; ref false in
    while not !crypt_loop do
        let str = Rsa.cryptMain (Printf.printf "Enter the second key (depends on whether the process is an encryption or a decryption) : "; read_int ()) (Printf.printf "Enter the first key (independent of process type : encryption/decryption) : "; read_int ()) (Printf.printf "Enter the value to encrypt/decrypt : "; read_int ()); print_string "\nHave another try ? (y/n) "; read_line () in
        if str.[0] = 'n' then crypt_loop := true
    done;    
  else print_endline "No valid mode chosen. Exiting.." ;;