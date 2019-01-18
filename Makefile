PINOPTS=-yn -k git

BUILD=dune build --profile=release
RUNTEST=dune runtest

.PHONY: all clean test bundle COMMIT exe ci

all:
	$(BUILD)

depends:
	opam pin add ${PINOPTS} datakit-client.dev .
	opam pin add ${PINOPTS} datakit-server.dev .
	opam pin add ${PINOPTS} datakit-client-9p.dev .
	opam pin add ${PINOPTS} datakit-server-9p.dev .
	opam pin add ${PINOPTS} datakit.dev .
	opam pin add ${PINOPTS} datakit-github.dev .
	opam pin add ${PINOPTS} datakit-bridge-github.dev .
	opam pin add ${PINOPTS} datakit-bridge-local-git.dev .
	opam pin add ${PINOPTS} datakit-ci.dev .
	opam install -y --deps-only datakit-ci datakit datakit-bridge-github datakit-bridge-local-git
	opam update -u datakit datakit-client datakit-server datakit-github \
	  datakit-ci datakit-bridge-github datakit-bridge-local-git -y

datakit:
	$(BUILD) -p datakit

client:
	$(BUILD) -p datakit-client

client-9p:
	$(BUILD) -p datakit-client-9p

server:
	$(BUILD) -p datakit-server

github:
	$(BUILD) -p datakit-github

bridge-local-git:
	$(BUILD) -p datakit-bridge-local-git

bridge-github:
	$(BUILD) -p datakit-bridge-github
	$(RUNTEST) test/datakit-github-bridge

ci:
	$(BUILD) -p datakit-ci
	$(RUNTEST) ci/tests

clean:
	dune clean
	rm -rf com.docker.db com.docker.db.exe COMMIT _tests
	rm -f examples/ocaml-client/*.native
	rm -f ci/skeleton/exampleCI.native
	rm -f com.docker.db

test:
	dune runtest

bundle:
	opam remove tls ssl -y
	$(MAKE) clean
	$(BUILD) src/datakit/bin/main.exe
	cp _build/default/src/datakit/bin/main.exe com.docker.db
	./scripts/check-dylib.sh

COMMIT:
	@git rev-parse HEAD > COMMIT

exe:
	opam remove tls ssl -y
	rm -rf _build/
	$(BUILD) src/datakit/bin/main.exe
	cp _build/default/src/datakit/bin/main.exe com.docker.db.exe
