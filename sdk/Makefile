OPAMYES=1

sdk:
	cd src && make clean
	docker build -t datakit-sdk .

mac:
	OPAMYES=1 opam pin add i9p.dev src/ -n
	OPAMYES=1 opam install i9p --deps
	OPAMYES=1 opam install i9p -t
