FROM ocaml/opam:alpine
RUN opam pin add protocol-9p git://github.com/djs55/ocaml-9p#0.5.0-beta2 -n
RUN opam depext protocol-9p irmin-unix
RUN opam install -y protocol-9p irmin-unix
COPY src /home/opam/src/i9p
RUN cd /home/opam/src/i9p && make clean
RUN opam pin add i9p.dev /home/opam/src/i9p
