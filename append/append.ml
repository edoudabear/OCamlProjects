let usage_msg = "append [-diophantienne]  [-premiers] [-crypt] ... ";;
let input_files = ref [];;
let anon_fun filename =
  input_files := filename :: !input_files;;
let diop_mode = ref false
and prem_mode = ref false
and crypt_mode = ref false;;
let speclist =
  [("-diophantienne", Arg.Set diop_mode, "Chooses to call diophantienne mode");
  ("-premiers",Arg.Set prem_mode,"Chooses to call premiers mode");
  ("-crypt",Arg.Set crypt_mode,"Allows encrypting and decrypting")]
let () =
  Arg.parse speclist anon_fun usage_msg;;
let main = if !diop_mode then print_endline "diophantienne mode selected" 
           else if !prem_mode then print_endline "premiers mode chosen" else print_endline "No mode chosen" ;;