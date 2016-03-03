VERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')
VFILE   = lib/version.ml

SETUP = ocaml setup.ml

build: setup.data $(VFILE)
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: $(VFILE)
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)
	rm -f $(VFILE)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

$(VFILE): _oasis
	echo "(** Datakit Version. *)" > $@
	echo "" >> $@
	echo "let current = \"$(VERSION)\"" >> $@
	echo "(** The current version of Datakit. *)" >> $@

init-doc:
	mkdir -p gh-pages
	cd gh-pages && ( \
	  git init && \
	  git remote add origin git@github.com:samoht/ciso.git && \
	  git fetch && \
	  git checkout -b gh-pages && \
	  (git pull origin gh-pages || exit 0))

update-doc: doc
	rm -f gh-pages/*.html
	cd gh-pages && cp ../ciso.docdir/*.html .
	cd gh-pages && git add * && (git commit -a -m "Update docs" || exit 0)
	cd gh-pages && git push origin gh-pages
