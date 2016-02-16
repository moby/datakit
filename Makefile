.PHONY: all sdk db static

all: sdk db
	@

sdk: sdk/src
	make -C sdk

db: db/src sdk
	make -C db

db: static/src sdk
	make -C static

pinata:
	git clone --depth 1 git@github.com:docker/pinata.git

sdk/src: pinata
	cd sdk && cp -r ../pinata/v1/i9p src

db/src: pinata
	cd db && cp -r ../pinata/v1/cmd/com.docker.db src

clean:
	rm -rf pinata sdk/src db/src
