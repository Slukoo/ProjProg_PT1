all: asyntax.cmo parser.cmo lexer.cmo compil.cmo tests.cmo 
	ocamlc asyntax.cmo parser.cmo lexer.cmo x86_64.cmo compil.cmo tests.cmo -o tests
	
out:
	gcc -no-pie out.s -o out

delout:
	rm -f out

clean:
	rm -f *.s  *.out *.cmo *.cmi parser.mli parser.ml lexer.ml tests out.s out


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


compil.cmo:  x86_64.cmo asyntax.cmo
	ocamlc -c compil.ml 


x86_64.cmo: x86_64.cmi
	ocamlc -c x86_64.ml

x86_64.cmi:
	ocamlc -c x86_64.mli
