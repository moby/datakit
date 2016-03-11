FROM ocaml/opam:alpine

RUN opam depext lwt tls && \
    opam install lwt tls alcotest                     # TLS support

COPY opam /home/opam/src/i9p/opam
RUN opam pin add i9p.dev /home/opam/src/i9p -n
RUN opam depext i9p && opam install i9p --deps        # Install i9p deps

COPY .git /home/opam/src/i9p/.git
COPY src /home/opam/src/i9p/src
COPY Makefile /home/opam/src/i9p/Makefile
COPY _oasis /home/opam/src/i9p/_oasis
COPY tests /home/opam/src/i9p/tests

RUN sudo chown -R opam.nogroup /home/opam/src
WORKDIR /home/opam/src/i9p

RUN opam config exec -- make && make test && make install

EXPOSE 5640

RUN sudo mkdir /data && sudo chown opam.nogroup /data && chmod 700 /data && \
    sudo cp /home/opam/.opam/system/bin/datakit /usr/bin/datakit

CMD ["/usr/bin/datakit", "--url=tcp://0.0.0.0:5640", "--git=/data"]