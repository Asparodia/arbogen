all : Unranking

Unranking : 
	ocamlopt -c Util.ml
	ocamlopt -c Grammar.ml
	ocamlopt -c Count.ml
	ocamlopt -c Unranking.ml
	ocamlopt -o unranking str.cmxa Util.cmx Grammar.cmx Count.cmx Unranking.cmx
	
clean :
	rm -f unranking *.o *.cmi *.cmx
