FROM ocaml/opam:alpine
RUN opam depext protocol-9p irmin-unix
RUN opam install -y protocol-9p irmin-unix
COPY src /home/opam/src/i9p
RUN opam pin add i9p.dev /home/opam/src/i9p
