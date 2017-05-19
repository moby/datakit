PINOPTS=-y -k git

BUILD=jbuilder build --dev
RUNTEST=jbuilder runtest

.PHONY: all clean test bundle COMMIT exe ci

all:
	$(BUILD)

depends:
	opam pin add ${PINOPTS} datakit-client .
	opam pin add ${PINOPTS} datakit-server .
	opam pin add ${PINOPTS} datakit .
	opam pin add ${PINOPTS} datakit-github .
	opam pin add ${PINOPTS} datakit-bridge-github .
	opam pin add ${PINOPTS} datakit-bridge-local-git .
	opam pin add ${PINOPTS} datakit-ci .
	opam update -u datakit datakit-client datakit-server datakit-github \
	  datakit-ci datakit-bridge-github datakit-bridge-local-git -y

datakit:
	$(BUILD) -n datakit
	$(RUNTEST) test/datakit

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
	rm -rf _build
	rm -rf com.docker.db com.docker.db.exe COMMIT _tests
	rm -f examples/ocaml-client/*.native
	rm -f ci/skeleton/exampleCI.native
	rm -f com.docker.db

test:
	jbuilder runtest --dev

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

REPO=../opam-repository
PACKAGES=$(REPO)/packages

# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	cd $(PACKAGES) && git add $*

opam-pkg:
	$(MAKE) pkg-datakit
	$(MAKE) pkg-datakit-client
	$(MAKE) pkg-datakit-server
	$(MAKE) pkg-datakit-ci
	$(MAKE) pkg-datakit-github
	$(MAKE) pkg-datakit-bridge-github
	$(MAKE) pkg-datakit-bridge-local-git
