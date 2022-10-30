all: asyntax.cmo parser.cmo lexer.cmo compil.cmo aritha.cmo 
	ocamlc asyntax.cmo parser.cmo lexer.cmo x86_64.cmo compil.cmo aritha.cmo -o aritha
	
out:
	gcc -no-pie out.s -o out

delout:
	rm -f out

clean:
	rm -f *.s  *.out *.cmo *.cmi parser.mli parser.ml lexer.ml aritha out.s out


aritha.cmo:
	ocamlc -c aritha.ml

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


compil.cmo:  x86_64.cmo asyntax.cmo compil.cmi
	ocamlc -c compil.ml 

compil.cmi:
	ocamlc -c compil.mli


x86_64.cmo: x86_64.cmi
	ocamlc -c x86_64.ml

x86_64.cmi:
	ocamlc -c x86_64.mli
