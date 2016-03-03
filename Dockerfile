FROM ocaml/opam:alpine
COPY opam /home/opam/src/i9p/opam
RUN opam pin add i9p.dev /home/opam/src/i9p -n
RUN opam depext i9p
RUN opam install alcotest
RUN opam install i9p --deps
COPY .git /home/opam/src/i9p/.git
COPY src /home/opam/src/i9p/src
COPY Makefile /home/opam/src/i9p/Makefile
COPY _oasis /home/opam/src/i9p/_oasis
COPY tests /home/opam/src/i9p/tests
RUN sudo chown -R opam.nogroup /home/opam/src
WORKDIR /home/opam/src/i9p
RUN opam config exec -- make
RUN opam config exec -- make test
ENTRYPOINT ["/home/opam/src/i9p/main.native"]
