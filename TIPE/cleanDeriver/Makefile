build :
	ocamlc -c src/deriveur.ml -o deriveur
	ocamlc -c src/convertisseur.ml -o convertisseur
	ocamlc -c src/canoniseur.ml -o canoniseur
	ocamlyacc -bcalc -v src/calc.mly
	ocamlc -c calc.mli -o calc.cmo
	ocamllex src/lexer.mll -o lexer.ml
	ocamlc -c lexer.ml -o lexer.cmo
	ocamlc -c calc.ml -o calc.cmo
	ocamlc -c src/main.ml -o main.cmo
	ocamlc -o calc deriveur.cmo convertisseur.cmo canoniseur.cmo lexer.cmo calc.cmo main.cmo
	rm deriveur.cmo convertisseur.cmo canoniseur.cmo calc.mli lexer.ml lexer.cmo calc.cmo main.cmo calc.ml
	rm calc.cmi convertisseur.cmi canoniseur.cmi calc.output deriveur.cmi lexer.cmi main.cmi	
	cp calc /home/edouda/server/expressjs/OCaml/
run :
	./calc
test :
	ocamlc -o tests src/tests.ml
	./tests
test+ :
	ocamlc -o tests src/tests.ml
	./tests d
