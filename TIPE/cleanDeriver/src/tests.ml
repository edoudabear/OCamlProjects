let count=ref 0 and fails=ref [] and i=ref 0;;

let tests=[

("1","1"), true;
("0","1"),false;
("3x^2+2","2+3xx"),true;
("e^1","exp(1)"),true;
("e^{x+1-2x*0,5}","exp(1)"),true];;

let n=ref 0;;

let f ((input1,input2),output) =
    let ev=Sys.command (List.fold_left String.cat "./calc compare \"" [input1;"\"  \"";input2;"\" "]) in
    if (if 0=ev then false else true)=output then
        incr count
    else fails := !i::!fails;
    incr i;;

let ()= List.iter f tests;
        Printf.printf "Tests summary : %d tests | %d/%d successful\n" !i !count !i;
        if Array.length Sys.argv > 1 && Sys.argv.(1)="d" then begin
            Printf.printf "Failed tests :\n";
            List.iter (fun v->Printf.printf "Test %d (line %d)\n" v (5+v)) !fails;
            Printf.printf "EOL\n"
        end;;