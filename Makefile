VERSION = $(shell git describe --match 'v[0-9]*' --dirty='.m' --always)
VFILE   = src/bin/version.ml
APP     = Datakit.app
PREFIX ?= $(shell opam config var prefix)

SETUP = ocaml setup.ml

build: setup.data $(VFILE)
	$(SETUP) -build $(BUILDFLAGS)

all: setup.data
	$(SETUP) -all $(ALLFLAGS)

setup.ml: _oasis
	rm -f _tags myocamlbuild.ml
	oasis setup
	echo 'true: debug, bin_annot' >> _tags
	echo 'true: warn_error(+1..49), warn(A-4-41-44)' >> _tags
	echo 'Ocamlbuild_plugin.mark_tag_used "tests"' >> myocamlbuild.ml

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test:
	$(SETUP) -configure --enable-tests --prefix $(PREFIX)
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
	rm -f src/*.odocl src/META setup.log
	rm -f src/**/META src/**/*.mldylib src/**/*.mllib
	rm -f $(VFILE)
	rm -rf $(APP) _tests

setup.data: setup.ml
	$(SETUP) -configure --prefix $(PREFIX)

bundle: build
	rm -rf $(APP)
	mkdir -p $(APP)/Contents/MacOS/
	mkdir -p $(APP)/Contents/Resources/lib/
	cp _build/src/bin/main.native $(APP)/Contents/MacOS/com.docker.db
	dylibbundler -od -b \
	 -x $(APP)/Contents/MacOS/com.docker.db \
	 -d $(APP)/Contents/Resources/lib \
	 -p @executable_path/../Resources/lib

$(VFILE): .git/refs/heads/master
	echo "let v = \"$(VERSION)\"" > $(VFILE)

www: doc
	cd www && cp ../datakit.docdir/*.html .
	if [ $(shell uname -s) == "Darwin" ]; then open www/index.html; fi

.PHONY: build doc test all install uninstall reinstall clean distclean
