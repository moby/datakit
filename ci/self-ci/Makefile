OCAMLBUILD_FLAGS=-use-ocamlfind

all: selfCI

%CI:
	ocamlbuild ${OCAMLBUILD_FLAGS} $@.native

docker:
	docker build -t editions/datakit-self-ci .

clean:
	ocamlbuild -clean
