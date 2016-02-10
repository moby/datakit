FROM ocaml/opam:debian-8_ocaml-4.02.3
USER root
RUN apt-get update && DEBIAN_FRONTEND=noninterative apt-get -y install -y libgmp-dev zlib1g-dev --no-install-recommends
USER opam
RUN opam pin add protocol-9p git://github.com/djs55/ocaml-9p#0.5.0-beta2
RUN opam install -y protocol-9p irmin-unix
COPY src /home/opam/src/i9p
RUN opam pin add i9p.dev /home/opam/src/i9p
