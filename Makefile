VERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')
VFILE   = src/bin/version.ml
APP     = Datakit.app
EXE     = Datakit.win
PREFIX ?= $(shell opam config var prefix)

SETUP = ocaml setup.ml

GITHUB ?= disable

build: setup.data $(VFILE) src/datakit.ml
	$(SETUP) -build $(BUILDFLAGS)

all: setup.data
	$(SETUP) -all $(ALLFLAGS)

setup.ml: _oasis _myocamlbuild.ml
	rm -f _tags myocamlbuild.ml
	oasis setup
	echo 'true: debug, bin_annot' >> _tags
	echo 'true: warn_error(+1..49), warn(A-4-41-44)' >> _tags
	echo '"$(APP)": -traverse' >> _tags
	echo '"$(EXE)": -traverse' >> _tags
	cat _myocamlbuild.ml >> myocamlbuild.ml

src/datakit.ml:
	echo "let () = ()" > src/datakit.ml

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test:
	$(SETUP) -configure --enable-tests --prefix $(PREFIX) --$(GITHUB)-github
	$(MAKE) build
	$(SETUP) -test $(TESTFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	if [ -f setup.ml ]; then $(SETUP) -clean $(CLEANFLAGS); fi
	rm -f setup.data setup.ml myocamlbuild.ml _tags configure
	rm -f src/bin/_tags
	rm -f src/*.odocl src/META setup.log
	rm -f src/**/META src/**/*.mldylib src/**/*.mllib
	rm -f $(VFILE)
	rm -rf $(APP) $(EXE) _tests
	rm -rf src/datakit.ml

setup.data: setup.ml
	$(SETUP) -configure --prefix $(PREFIX) --$(GITHUB)-github

bundle:
	opam remove tls ssl -y
	$(MAKE) clean
	$(MAKE) GITHUB=disable
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
	$(MAKE) clean
	$(MAKE) GITHUB=disable
	rm -rf $(EXE)
	mkdir -p $(EXE)
	cp _build/src/bin/main.native $(EXE)/datakit.exe
	cp /usr/x86_64-w64-mingw32/sys-root/mingw/bin/zlib1.dll $(EXE)

ifeq ($(wildcard .git/refs/heads/master),)
$(VFILE):
	echo "let v = \"$(VERSION)\"" > $(VFILE)
else
GIT_VERSION = $(shell git describe --match 'v[0-9]*' --dirty='.m' --always)
$(VFILE): .git/refs/heads/master
	echo "let v = \"$(GIT_VERSION)\"" > $(VFILE)
endif

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push upstream v$(VERSION)

www: doc
	cd www && cp ../datakit.docdir/*.html .
	if [ $(shell uname -s) == "Darwin" ]; then open www/index.html; fi

gh-pages/.git:
	mkdir -p gh-pages
	cd gh-pages && git init
	cd gh-pages && git remote add origin git@github.com:docker/datakit.git
	cd gh-pages && git fetch origin
	cp www/style.css gh-pages
	cd gh-pages && (git checkout -b gh-pages && git add style.css && git commit -a -m init || ok)

gh-pages: gh-pages/.git doc
	cd gh-pages && (git checkout gh-pages || ok)
	rm -f gh-pages/*.html
	cp datakit.docdir/*.html gh-pages
	cp www/style.css gh-pages
	cd gh-pages && git add *.html
	cd gh-pages && git commit -a -m "Doc updates"
	cd gh-pages && git push origin gh-pages

.PHONY: build doc test all install uninstall reinstall clean distclean gh-pages
