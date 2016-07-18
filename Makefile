GITHUB=$(shell opam config var github:installed)
APP=Datakit.app
EXE=Datakit

COMMIT_ID=$(shell git rev-parse HEAD)
TESTS = true

.PHONY: all clean test bundle COMMIT exe

all:
	ocaml pkg/pkg.ml build --tests $(TESTS) --with-github $(GITHUB)

clean:
	rm -rf _build $(APP) $(EXE)
	rm -f com.docker.db

test:
	ocaml pkg/pkg.ml build --tests true --with-github $(GITHUB)
	ocaml pkg/pkg.ml test

bundle:
	opam remove tls ssl -y
	$(MAKE) clean
	ocaml pkg/pkg.ml build --tests false --with-github false --pinned true
	mkdir -p $(APP)/Contents/MacOS/
	mkdir -p $(APP)/Contents/Resources/lib/
	cp _build/src/bin/main.native $(APP)/Contents/MacOS/com.docker.db
	./scripts/check-dylib.sh
	dylibbundler -od -b \
	 -x $(APP)/Contents/MacOS/com.docker.db \
	 -d $(APP)/Contents/Resources/lib \
	 -p @executable_path/../Resources/lib
	cp $(APP)/Contents/MacOS/com.docker.db .


COMMIT:
	@echo $(COMMIT_ID) > COMMIT

exe:
	opam remove tls ssl -y
	$(MAKE) clean
	ocaml pkg/pkg.ml build --tests false --with-github false --pinned true
	mkdir -p $(EXE)
	cp _build/src/bin/main.native $(EXE)/datakit.exe
	cp /usr/x86_64-w64-mingw32/sys-root/mingw/bin/zlib1.dll $(EXE)
