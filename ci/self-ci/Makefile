OCAMLBUILD_FLAGS=-use-ocamlfind

all: selfCI

%CI:
	ocamlbuild ${OCAMLBUILD_FLAGS} $@.native

clean:
	ocamlbuild -clean
