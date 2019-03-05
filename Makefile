OCB = ocamlbuild -use-ocamlfind

all: arbogen lib

arbogen:
	$(OCB) src/Arbogen.native
	$(OCB) src/Arbogen.byte
	@mkdir -p bin
	@mv Arbogen.native bin/arbogen.native
	@mv Arbogen.byte bin/arbogen.byte

lib:
	$(OCB) src/Arbolib.cmxa
	$(OCB) src/Arbolib.cma
	$(OCB) src/Arbolib.cmi
	@mkdir -p lib
	@cp _build/src/Arbolib.cma lib
	@cp _build/src/Arbolib.cmxa lib
	@cp _build/src/Arbolib.cmi lib
	@cp _build/src/Arbolib.a lib
	@cp _build/src/Arbolib.o lib
	@cp _build/src/Arbolib.cmo lib
	@cp _build/src/Arbolib.cmx lib

install-lib: lib uninstall
	ocamlfind install arbolib META lib/*

uninstall-lib:
	ocamlfind remove arbolib

.PHONY: all install-lib clean lib uninstall

clean:
	rm -rf _build bin lib
