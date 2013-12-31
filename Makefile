all: lex.native

lex.native: lex.ml lex.mli
	ocamlfind ocamlc -w +a-4-44 -package batteries -linkpkg lex.mli lex.ml -o lex.native

clean:
	$(RM) lex *.cmi *.cmo

