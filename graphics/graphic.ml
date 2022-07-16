(* ocamlfind ocamlopt -package graphics -linkpkg -g graphic.ml -o graphic *)
open Graphics;;
open_graph " 480x270"
;set_window_title "Diskloud Window";;
remember_mode false;
try
  while true do
    let st = wait_next_event [ Mouse_motion; Button_down; Key_pressed ] in
    synchronize ();
    if st.button then (
      remember_mode true;
      draw_rect st.mouse_x st.mouse_y 1 1;
      remember_mode false);
    if st.keypressed then raise Exit;
    let x = st.mouse_x + 16 and y = st.mouse_y + 16 in
    moveto x y;
    draw_string "Diskloud ©";
  done
with Exit -> ()

(*
function onLPressed ( event ) {
    if (event.code != "KeyL") {
        return;
    }
    if (first[0] == null) {
        first = mousePos;
        return;
    }else {
        last = mousePos;
    }
    let steps = Math.abs(last[0]-first[0]) + Math.abs(last[1]-first[1]);
    steps = Math.ceil(steps/4);
    let points = [(last[0]-first[0])/steps,(last[1]-first[1])/steps]
        
    for (let k=0; k<steps; k++) {

        createSquare(first[0] + (points[0]*k), first[1] + (points[1]*k))
    }
    first = [null,null];
    last = [null,null];
}   
*)