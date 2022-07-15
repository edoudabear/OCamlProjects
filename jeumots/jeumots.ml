exception JMException of string;;
open Printf;;
let () = Random.self_init();;
let usage_msg = " [-nWord] [-play] ... ";;
let input_files = ref [];;
let anon_fun filename =
  input_files := filename :: !input_files;;
let newWord = ref "";;
let play = ref false;;
let speclist =
  [("-nWord", Arg.Set_string newWord, "Allows to add a new word");
   ("-play",Arg.Set play,"Starts game")];;

let file = "_mots";;

let () =
  Arg.parse speclist anon_fun usage_msg;;

let print_boolean = function
| true -> print_endline "true"
| false -> print_endline "false";;

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

let main = 
  if !newWord<>"" then (
      (* Write message to file *)
      let oc = open_out_gen [Open_append] (0o777) (file) in    (* create or truncate file, return channel *)
      fprintf oc "%s\n" (String.uppercase_ascii (!newWord));  (* write something *)   
      close_out oc;                (* flush and close the channel *)
  ) 
  else if !play then (
    let ic = open_in file in (
      try (
        let lineCount =
          let rec aux line =
            try (
              1+ aux (input_line ic);
          ) with e-> (
            1
          )
          in aux (input_line ic);
        in print_endline ((string_of_int lineCount)^" Words.");print_endline ("Chosen word : "^(wordNo "_mots" (Random.int lineCount)))

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