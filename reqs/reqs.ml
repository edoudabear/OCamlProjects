(*
COMPILING COMMAND : ocamlfind ocamlc -package quests,ppx_yojson,lwt_ppx -linkpkg -o reqs -thread reqs.ml
*)
open Lwt;;
open Yojson;;

let get () =
  print_endline "Performing req 1.." ; Quests.get "https://e.diskloud.fr/spotipyAPI"
    ~params:[ ("key1", "value1"); ("key2", "value2") ]
  >|= Quests.Response.show >|= print_endline;;

let post_form () =
  print_endline "Performing req 2.." ; Quests.post "https://e.diskloud.fr/Dilab/get" ~data:(Form [ ("type", "mainGroups") ])
  >|= Quests.Response.show >|= print_endline;;

let post_json () =
  Quests.post "https://e.diskloud.fr/Dilab/get" ~data:(Json [%yojson { content = "test" }])
  >|= Quests.Response.show >|= print_endline;;

let gzip_response () =
  print_endline "Performing req 3.." ; Quests.get "https://e.diskloud.fr/spotipyAPI"
  >|= Quests.Response.show >|= print_endline;;

let following_redirects () =
  print_endline "Performing req 4.." ; Quests.get "https://e.diskloud.fr/spotipyAPI"
  >|= Quests.Response.show >|= print_endline;;

let basic_authentication () =
  print_endline "Performing req 5.." ; Quests.get "https://e.diskloud.fr/spotipyAPI" ~auth:(Basic ("Edouda", "test"))
  >|= Quests.Response.show >|= print_endline;;

let bearer_authentication () =
  print_endline "Performing req 6.." ; Quests.get "https://example.com/some-api" ~auth:(Bearer "a token")
  >|= Quests.Response.show >|= print_endline;;

let sessions () =
  let open Quests in
  let s = Session.create () in
  let%lwt () =
    Session.get s "https://example.com" >|= Response.show >|= print_endline
  in
  Session.close s;;

let () =
  Lwt_main.run
    (Lwt_list.iter_s
       (fun f -> f ())
       [
         get;
         post_form;
         post_json;
         gzip_response;
         following_redirects;
         basic_authentication;
         bearer_authentication;
         sessions;
       ]);;