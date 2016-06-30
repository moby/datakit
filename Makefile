GITHUB=$(shell opam config var github:installed)
APP=Datakit.app
EXE=Datakit

all:
	ocaml pkg/pkg.ml build --with-github $(GITHUB)

clean:
	rm -rf _build

test:
	ocaml pkg/pkg.ml build --tests true --with-github $(GITHUB)
	ocaml pkg/pkg.ml test

bundle:
	opam remove tls ssl -y
	$(MAKE) clean
	$(MAKE) GITHUB=false
	rm -rf $(APP)
	mkdir -p $(APP)/Contents/MacOS/
	mkdir -p $(APP)/Contents/Resources/lib/
	cp _build/src/bin/main.native $(APP)/Contents/MacOS/com.docker.db
	./scripts/check-dylib.sh
	dylibbundler -od -b \
	 -x $(APP)/Contents/MacOS/com.docker.db \
	 -d $(APP)/Contents/Resources/lib \
	 -p @executable_path/../Resources/lib

exe:
	opam remove tls ssl -y
	rm -rf _build
	ocaml pkg/pkg.ml build --tests false --with-github false
	rm -rf $(EXE)
	mkdir -p $(EXE)
	cp _build/src/bin/main.native $(EXE)/datakit.exe
	cp /usr/x86_64-w64-mingw32/sys-root/mingw/bin/zlib1.dll $(EXE)
