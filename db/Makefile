sdk:
	cd src && make clean
	cd src && make version.ml
	docker build -t datakit-db .

mac:
	cd ../sdk && make make
	OPAMYES=1 opam pin add db.dev src/

src/version.ml:
	cd src && make version
