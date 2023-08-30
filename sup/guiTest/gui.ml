(*
BEFORE RUNNING THE PROGRAM : export SDL_VIDEO_X11_VISUALID=
COMPILING COMMAND : ocamlfind ocamlc -package bogue,quests -linkpkg -o minimal -thread minimal.ml
*)

open Bogue
open Lwt;;
open Yojson;;
module W = Widget
module L = Layout

let main () =
  let title = W.label "Diskloud" ~size:20 ~fg:(0,150,255,0) in
  let loginLbl = W.label "Login" in
  let usrInput = W.text_input () ~prompt:"username" in
  let psdInput = W.text_input () ~prompt:"password" in
  let loginBtn = W.button "Log in" in
  let prvtCheck = W.check_box () in
  let prvtLbl = W.label "I Agree to the Diskloud policy" in
  let titleL = L.flat_of_w [title] ~h:20 in
  let loginLblL = L.flat_of_w [loginLbl] ~sep:0 in
  let usrInputL = L.flat_of_w [usrInput] ~sep:0 in
  let psdInputL = L.flat_of_w [psdInput] ~sep:0 in
  let loginBtnL = L.flat_of_w [loginBtn] ~sep:0 in
  let prvtL = L.flat_of_w [prvtCheck;prvtLbl] in
  let layout = L.tower ~scale_content:false ~sep:1 ~align:Draw.Center [titleL; loginLblL; usrInputL; psdInputL; prvtL; loginBtnL] in

  let action ti l _ =
    if W.get_state prvtCheck=false then (
      Popup.info ("Error\nYou must agree to the Diskloud Policy") layout
    ) else (
      (* print_endline "Performing request.."; Quests.post "https://e.diskloud.fr/Dilab/connect" ~data:(Json [%yojson { username = [%aq `String (W.get_text usrInput)] ; password = [%aq `String (W.get_text psdInput)] }]) >|= Quests.Response.content >|= print_endline; *)
      Popup.info 
      ("Data\nUsername : "^(W.get_text usrInput)^"\nPassword : "^(W.get_text psdInput)^"\n\nOutput :\n")
      layout
    ) in
  let c = W.connect loginBtn loginBtn action Trigger.buttons_down in
  (* let p = W.connect psdInput psdInput (W.map_text (fun h->"*")) Trigger.[text_input;key_up] in *)

  let board = Bogue.make [c] [layout] in
  Bogue.run board ;;

let () = main ();
  Bogue.quit ()