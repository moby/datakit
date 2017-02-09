OCAMLBUILD_FLAGS=-use-ocamlfind

all: selfCI

%CI:
	ocamlbuild ${OCAMLBUILD_FLAGS} $@.native

# To keep the final image small, we copy the binary from the build image to a fresh base
docker:
	docker build -t self-ci-dev .
	docker run --rm -i self-ci-dev cat _build/selfCI.native > docker-self-ci
	chmod a+x docker-self-ci
	docker build -t editions/datakit-self-ci -f Dockerfile.run .

clean:
	ocamlbuild -clean
