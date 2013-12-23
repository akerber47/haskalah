all: lex

lex: lex.ml
	ocamlc -o lex lex.ml

clean:
	$(RM) lex *.cmi *.cmo

