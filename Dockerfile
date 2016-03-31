FROM ocaml/opam:alpine

RUN opam depext lwt ssl &&  opam install lwt alcotest

RUN opam pin add github https://github.com/samoht/ocaml-github.git#status-context
RUN opam pin add protocol-9p --dev

COPY . /home/opam/src/datakit
RUN opam pin add i9p.dev /home/opam/src/datakit -n
RUN opam depext i9p && opam install i9p --deps        # Install i9p deps

RUN sudo chown -R opam.nogroup /home/opam/src
WORKDIR /home/opam/src/datakit

RUN opam config exec -- make && make test && make install

EXPOSE 5640

RUN sudo mkdir /data && sudo chown opam.nogroup /data && chmod 700 /data && \
    sudo cp /home/opam/.opam/system/bin/datakit /usr/bin/datakit

CMD ["/usr/bin/datakit", "--url=tcp://0.0.0.0:5640", "--git=/data", "--verbose=debug"]
