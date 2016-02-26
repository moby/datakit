OPAMYES=1

sdk:
	cd src && make clean
	docker build -t datakit-sdk .

mac:
	OPAMYES=1 opam pin add protocol-9p git://github.com/djs55/ocaml-9p#0.5.0-beta4 -n
	OPAMYES=1 opam pin add i9p.dev src/
