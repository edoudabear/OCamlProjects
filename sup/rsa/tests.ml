(*ocamlfind ocamlopt -o rsaTests -package oUnit -linkpkg -g rsa.ml tests.ml*)
open OUnit2;;

let coeffs = function
| (a,b,c,d) -> (c,d);;

let test1 test_ctxt = assert_equal (1,-1) (coeffs (Rsa.diophantienne (5) (6)));;

let suite =
    "suite">:::
     ["test1">:: test1]
;;

let () =
    run_test_tt_main suite
;;