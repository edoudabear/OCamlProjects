run :
	ocamlc -c deriveur.ml
	ocamlyacc -v calc.mly
	ocamlc -c calc.mli
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c calc.ml
	ocamlc -c main.ml
	ocamlc -o calc deriveur.cmo lexer.cmo calc.cmo main.cmo
	rm calc.mli lexer.ml calc.ml
