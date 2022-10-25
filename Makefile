all: asyntax.cmo parser.cmo lexer.cmo tests.cmo
	ocamlc asyntax.cmo parser.cmo lexer.cmo tests.cmo -o tests
	

clean:
	rm -f *.s  *.out *.cmo *.cmi parser.mli parser.ml lexer.ml


tests.cmo:
	ocamlc -c tests.ml

lexer.cmo: lexer.ml parser.cmi
	ocamlc -c lexer.ml

lexer.ml:
	ocamllex lexer.mll

parser.cmo: parser.cmi
	ocamlc -c parser.ml

parser.cmi: parser.mli asyntax.cmo
	ocamlc -c parser.mli

parser.mli:
	ocamlyacc parser.mly

asyntax.cmo: asyntax.cmi
	ocamlc -c asyntax.ml

asyntax.cmi:
	ocamlc -c asyntax.mli






compil:  x86_64.cmo
	ocamlc x86_64.cmo compil.ml -o compil.out


x86_64.cmo: x86_64.cmi
	ocamlc -c x86_64.ml

x86_64.cmi:
	ocamlc -c x86_64.mli
