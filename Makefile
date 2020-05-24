
parse:
	ocamlyacc parse.mly
	ocamlc -c parse.mli
	ocamllex lexy.mll
	ocamlc -c lexy.ml
	ocamlc -c str.cma parse.ml
	ocamlc -c mainf.ml
	ocamlc -o parse str.cma lexy.cmo parse.cmo mainf.cmo
	rm parse.mli lexy.ml parse.ml
clean:
	rm *.cmo lex.ml parse.ml *.cmi *.mli parse