exception JMException of string;;

open Printf;;

let () = Random.self_init();;

let usage_msg = " [-nWord] [-play] ... ";;

let input_files = ref [];;

let anon_fun filename = input_files := filename :: !input_files;;

let newWord = ref "";;

let play = ref false;;

let speclist =
  [("-nWord", Arg.Set_string newWord, "Allows to add a new word");
   ("-play",Arg.Set play,"Starts game")];;

let file = "_mots";;

let () = Arg.parse speclist anon_fun usage_msg;;

let print_boolean = function
| true -> print_endline "true"
| false -> print_endline "false";;

let str_to_list str = List.filter (fun h -> if h<>' ' then true else false) (List.init (String.length str) (fun t -> Char.uppercase_ascii  str.[t]));;

let filter_by_char cha lst = List.filter (fun h -> if h<>cha then true else false) lst;;

let rec (>>) value = function
| h::t -> if value=h then true else value >> t
| [] -> false;;

let rec uniDelete value = function
| [] -> []
| h :: t -> if h=value then t else h :: (uniDelete value t);;

let rec unify = function
| [] -> []
| h :: t -> if h >> t then unify t else h :: unify t;;

let wordNo fileName line =
  let ic = open_in fileName in (
    try 
      let rec lineSeek lineContent line=
        try
          match line with
          | 0 -> lineContent
          | _ -> lineSeek (input_line ic) (line - 1)
        with e ->
           close_in ic;                 (* close the input channel *) 
           raise (JMException "Invalid line count");
    in lineSeek (input_line ic) (line);
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    raise e
  );;

let convertWordToStar word letters = String.map (fun h -> if h >> letters then '*' else h) (word);;

let exitConfirm ansr =if (Char.uppercase_ascii ansr.[0])=='Y' || (Char.uppercase_ascii ansr.[0])=='O' then
                        true
                      else false;;

let lineCount =
  let ic = open_in file in
    let rec aux line =
      try (
        try (
          1+ aux (input_line ic);
      ) with e-> (
        1
      )
    )
    with e -> (                    (* some unexpected exception occurs *)
      close_in_noerr ic;           (* emergency closing *)
      raise e
    ) in aux (input_line ic)

let printCharList x = print_char '['; 
  let rec printCharInside = function
  | [] -> print_char ']';
  | h::t -> print_char h;if t!=[] then print_string " ; ";printCharInside t;
in printCharInside x ; print_char '\n';;

let rec wordGuess word =
  let rec main guessWord letters userInput=
  try
    (* printCharList letters; *)
    if (uniDelete (Char.uppercase_ascii userInput.[0]) letters)=[] then (
      Printf.printf "Et c'est gagné, vous avez trouvé le  mot : %s !\n" guessWord; wordGuess (wordNo "_mots" (Random.int lineCount))
    ) else if (Char.uppercase_ascii userInput.[0]) >> letters then (
      print_endline "Valide, cette lettre est juste !";
      main guessWord (uniDelete (Char.uppercase_ascii userInput.[0]) letters) (Printf.printf "Revoici le mot : %s. Choisissez une nouvelle lettre\n" (convertWordToStar word (uniDelete (Char.uppercase_ascii userInput.[0]) letters)); read_line())
    ) else (
      print_endline "Et non, raté.";
      main guessWord (uniDelete (Char.uppercase_ascii userInput.[0]) letters) (Printf.printf "Revoici le mot : %s. Choisissez une nouvelle lettre\n" (convertWordToStar word (uniDelete (Char.uppercase_ascii userInput.[0]) letters)); read_line())
    )
    with e -> if exitConfirm (print_endline "Vous avez mal tapé, voulez vous quitter le jeu (y/n) ?"; read_line ()) then exit 0 else main (guessWord) (letters) (Printf.printf "Devinez quel mot se cache derrière %s\n" (convertWordToStar word (uniDelete (Char.uppercase_ascii userInput.[0]) letters)); read_line())
  in main word (str_to_list word |> filter_by_char ' ' |> unify) (Printf.printf "Devinez quel mot se cache derrière %s\n" (convertWordToStar word (str_to_list word |> filter_by_char ' ' |> unify)); read_line ()) ;;

let rec wordPlay word lifeCount =
  let rec main guessWord letters userInput remainingLifes=
  try
    (* printCharList letters; *)
    (* Printf.printf "%d vie(s) restante(s)" remainingLifes; *)
    if remainingLifes-1=0 then (
      Printf.printf "R.I.P.\nTu es mort. Le mot était : %s. Changeons de mot" guessWord; wordPlay (wordNo "_mots" (Random.int lineCount)) 15
    ) else if (uniDelete (Char.uppercase_ascii userInput.[0]) letters)=[] then (
      Printf.printf "Et c'est gagné, vous avez trouvé le  mot : %s !\n" guessWord; wordPlay (wordNo "_mots" (Random.int lineCount)) 15
    ) else if (Char.uppercase_ascii userInput.[0]) >> letters then (
      print_endline "Valide, cette lettre est juste !";
      main guessWord (uniDelete (Char.uppercase_ascii userInput.[0]) letters) (Printf.printf "%d vie(s) restante(s)\nRevoici le mot : %s. Choisissez une nouvelle lettre\n" (remainingLifes) (convertWordToStar word (uniDelete (Char.uppercase_ascii userInput.[0]) letters)); read_line()) (remainingLifes)
    ) else (
      print_endline "Et non, raté.";
      main guessWord (uniDelete (Char.uppercase_ascii userInput.[0]) letters) (Printf.printf "%d vie(s) restante(s)\nRevoici le mot : %s. Choisissez une nouvelle lettre\n" (remainingLifes-1) (convertWordToStar word (uniDelete (Char.uppercase_ascii userInput.[0]) letters)); read_line()) (remainingLifes-1)
    )
    with e -> if exitConfirm (print_endline "Vous avez mal tapé, voulez vous quitter le jeu (y/n) ?"; read_line ()) then exit 0 else main (guessWord) (letters) (Printf.printf "%d vie(s) restante(s)\nDevinez quel mot se cache derrière %s\n" (remainingLifes) (convertWordToStar word (letters)); read_line()) (remainingLifes)
  in main word (str_to_list word |> filter_by_char ' ' |> unify) (Printf.printf "%d vie(s) restante(s)\nDevinez quel mot se cache derrière %s\n" (lifeCount) (convertWordToStar word (str_to_list word |> filter_by_char ' ' |> unify)); read_line ()) (lifeCount);;

let wordList fileName =
  let ic = open_in fileName in (
    try 
      let rec lineSeek lineContent =
        try
          lineContent :: lineSeek (input_line ic)
        with e -> [lineContent]
    in lineSeek (input_line ic);
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    raise e
  );;

let main = 
  if !newWord<>"" then (
      (* Write message to file *)
      let oc = open_out_gen [Open_append] (0o777) (file) in    (* create or truncate file, return channel *)
      if not (String.uppercase_ascii !newWord >> wordList file) then (
          fprintf oc "%s\n" (String.uppercase_ascii (!newWord));  (* write something *)   
          close_out oc;                (* flush and close the channel *)
        ) else (
          print_endline "The word is already in the guesslist !";
          close_out oc;                (* flush and close the channel *)
        )
  ) 
  else if !play then (
    let ic = open_in file in (
      try (
        print_endline ((string_of_int lineCount)^" Words."); wordPlay (wordNo "_mots" (Random.int lineCount)) 15
      )
      with e -> (                    (* some unexpected exception occurs *)
        close_in_noerr ic;           (* emergency closing *)
        raise e
      )
    )
  )
  else (
    (* Read file and display the first line *)
    let ic = open_in file in
      try 
        let rec lineSeek line =
            try
              print_endline line;          (* write the result to stdout *)
              flush stdout;                (* write on the underlying device now *)
              lineSeek (input_line ic);
            with e ->
               close_in ic;                 (* close the input channel *) 
               print_endline "End of file reached";
        in lineSeek (input_line ic);
      with e ->                      (* some unexpected exception occurs *)
        close_in_noerr ic;           (* emergency closing *)
        raise e                      (* exit with error: files are closed but
                                        channels are not flushed *)
  )
  
  
  (* normal exit: all channels are flushed and closed *)