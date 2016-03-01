.PHONY: all sdk db static

all: sdk db
	@

mac:
	cd db && make mac

sdk:
	make -C sdk

db:
	make -C db

I9P=$(shell which i9p)

bundle: mac
	rm -rf root
	mkdir -p root/Contents/MacOS/
	mkdir -p root/Contents/Resources/lib/
	echo $(I9P)
	cp $(I9P) root/Contents/MacOS/com.docker.db
	dylibbundler -od -b \
	  -x root/Contents/MacOS/com.docker.db \
	  -d root/Contents/Resources/lib \
	  -p @executable_path/../Resources/lib

static: sdk
	make -C static

clean:
	@rm -rf pinata

release:
	if [ -z "$(VERSION)" ]; then echo "VERSION is not set"; exit 1; fi
	git tag $(VERSION) -m "Version $(VERSION)"
	git push upstream master $(VERSION)
