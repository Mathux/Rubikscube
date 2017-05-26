all:
	ocamlbuild  -tag thread -use-ocamlfind -package graphics -package unix draw.native
	mv draw.native draw

	ocamlbuild  -tag thread -use-ocamlfind -package graphics -package unix biks.native
	mv biks.native biks
clean:
	ocamlbuild -clean
