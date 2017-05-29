all:
	ocamlbuild  -tag thread -use-ocamlfind -package graphics -package unix main.native
	mv main.native main

#	ocamlbuild  -tag thread -use-ocamlfind -package graphics -package unix biks.native
#	mv biks.native biks
clean:
	ocamlbuild -clean
