(*
COMPILING COMMAND : ocamlfind ocamlc -package quests,core,ppx_yojson,lwt_ppx -linkpkg -o reqs -thread reqs.ml
*)
open Lwt;;
open Yojson;;

let get () =
  print_endline "Performing req 1.." ; Quests.get "http://httpbin.org/get"
    ~params:[ ("key1", "value1"); ("key2", "value2") ]
  >|= Quests.Response.show >|= print_endline;;

let post_form () =
  print_endline "Performing req 2.." ; Quests.post "http://httpbin.org/post" ~data:(Form [ ("key", "value") ])
  >|= Quests.Response.show >|= print_endline;;

let post_json () =
  Quests.post "http://httpbin.org/post" ~data:(Json [%yojson { key = "value" }])
  >|= Quests.Response.show >|= print_endline;;

let gzip_response () =
  print_endline "Performing req 3.." ; Quests.get "http://httpbin.org/gzip"
  >|= Quests.Response.show >|= print_endline;;

let following_redirects () =
  print_endline "Performing req 4.." ; Quests.get "http://httpbin.org/redirect/1"
  >|= Quests.Response.show >|= print_endline;;

let basic_authentication () =
  print_endline "Performing req 5.." ; Quests.get "https://postman-echo.com/basic-auth" ~auth:(Basic ("username", "password"))
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
         (* get; *)
         post_form;
         post_json;
         gzip_response;
         following_redirects;
         basic_authentication;
         bearer_authentication;
         sessions;
       ]);;