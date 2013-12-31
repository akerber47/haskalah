all: lex.native test_lex.native

lex.native: lex.ml lex.mli
	ocamlfind ocamlc -w +a-4-44 -package batteries -linkpkg lex.mli lex.ml -o lex.native

test_lex.native: lex.ml lex.mli test/test_lex.ml
	ocamlfind ocamlc -w +a-4-44 -package batteries -package oUnit -linkpkg lex.mli lex.ml test/test_lex.ml -o test_lex.native

clean:
	$(RM) *.cmi *.cmo *.native
	cd test && $(RM) *.cmi *.cmo *.native

