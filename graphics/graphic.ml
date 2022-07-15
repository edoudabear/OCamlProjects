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