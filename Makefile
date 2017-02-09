GITHUB=$(shell opam config var github:installed)
APP=Datakit.app
EXE=Datakit
PINOPTS=-y -k git

TESTS = true

.PHONY: all clean test bundle COMMIT exe ci

all: datakit
	@

depends:
	opam pin add ${PINOPTS} datakit-client .
	opam pin add ${PINOPTS} datakit-server .
	opam pin add ${PINOPTS} datakit-github .
	opam pin add ${PINOPTS} datakit-ci .
	opam update -u datakit-client datakit-server datakit-github datakit-ci -y

datakit:
	ocaml pkg/pkg.ml build --tests $(TESTS) -q

client:
	ocaml pkg/pkg.ml build -n datakit-client -q

server:
	ocaml pkg/pkg.ml build -n datakit-server -q

github:
	ocaml pkg/pkg.ml build -n datakit-github -q

bridge-local-git:
	ocaml pkg/pkg.ml build -n datakit-bridge-local-git -q

bridge-github:
	ocaml pkg/pkg.ml build -n datakit-bridge-github -q --tests true
	ocaml pkg/pkg.ml test

ci:
	ocaml pkg/pkg.ml build -n datakit-ci -q --tests true
	ocaml pkg/pkg.ml test _build/ci/tests/test_ci.native

clean:
	ocaml pkg/pkg.ml clean
	rm -rf $(APP) $(EXE) _tests
	rm -f examples/ocaml-client/*.native
	rm -f ci/skeleton/exampleCI.native
	rm -f com.docker.db

test:
	ocaml pkg/pkg.ml build --tests true
	ocaml pkg/pkg.ml test

bundle:
	opam remove tls ssl -y
	$(MAKE) clean
	ocaml pkg/pkg.ml build --tests false --pinned true
	mkdir -p $(APP)/Contents/MacOS/
	mkdir -p $(APP)/Contents/Resources/lib/
	cp _build/src/datakit/main.native $(APP)/Contents/MacOS/com.docker.db
	./scripts/check-dylib.sh
	dylibbundler -od -b \
	 -x $(APP)/Contents/MacOS/com.docker.db \
	 -d $(APP)/Contents/Resources/lib \
	 -p @executable_path/../Resources/lib
	cp $(APP)/Contents/MacOS/com.docker.db .

COMMIT:
	@git rev-parse HEAD > COMMIT

exe:
	opam remove tls ssl -y
	rm -rf _build/
	ocaml pkg/pkg.ml build --tests false --pinned true
	mkdir -p $(EXE)
	cp _build/src/datakit/main.native $(EXE)/datakit.exe
	cp /usr/x86_64-w64-mingw32/sys-root/mingw/bin/zlib1.dll $(EXE)
