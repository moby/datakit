OCAMLBUILD_FLAGS=-use-ocamlfind

all: exampleCI

%CI:
	ocamlbuild ${OCAMLBUILD_FLAGS} $@.native

clean:
	ocamlbuild -clean
