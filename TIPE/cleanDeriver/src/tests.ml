let count=ref 0 and fails=ref [] and i=ref 0;;

let tests=[

("1","1","compare"), true;
("0","1","compare"),false;
("3x^2+2","2+3xx","compare"),true;
("e^1","exp(1)","compare"),true;
("e^{x+1-2x*0,5}","exp(1)","compare"),true;
("2x+3","2cos(0)","test_entree_utilisateur"),true;
("exp(3x^3)+tan(3x)","3/(cos(3x)cos(3x))+9xxexp(3x^3)","test_entree_utilisateur"),true;
("\\frac{25x+3}{17x^2}","(25*17xx-34*x(25x+3))/(17*17xxxx)","test_entree_utilisateur"),true
];;

let n=ref 0;;

let f ((input1,input2,queryType),output) =
    let ev=Sys.command (List.fold_left String.cat "./calc " [queryType;" \"";input1;"\"  \"";input2;"\" "]) in
    if (if 0=ev then false else true)=output then
        incr count
    else fails := !i::!fails;
    incr i;;

let ()= List.iter f tests;
        Printf.printf "Tests summary : %d tests | %d/%d successful\n" !i !count !i;
        if Array.length Sys.argv > 1 && Sys.argv.(1)="d" then begin
            Printf.printf "Failed tests :\n";
            List.iter (fun v->Printf.printf "Test %d (line %d)\n" (v+1) (5+v)) !fails;
            Printf.printf "EOL\n"
        end;;