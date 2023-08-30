build :
	ocamlc -c src/deriveur.ml -o deriveur
	ocamlyacc -bcalc -v src/calc.mly
	ocamlc -c calc.mli -o calc.cmo
	ocamllex src/lexer.mll -o lexer.ml
	ocamlc -c lexer.ml -o lexer.cmo
	ocamlc -c calc.ml -o calc.cmo
	ocamlc -c src/main.ml -o main.cmo
	ocamlc -o calc deriveur.cmo lexer.cmo calc.cmo main.cmo
	rm deriveur.cmo calc.mli lexer.ml lexer.cmo calc.cmo main.cmo calc.ml
	rm calc.cmi calc.output deriveur.cmi lexer.cmi main.cmi
run :
	./calc
