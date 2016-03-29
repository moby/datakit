FROM ocaml/opam:alpine

RUN opam depext lwt &&  opam install lwt alcotest

COPY opam /home/opam/src/datakit/opam
RUN opam pin add i9p.dev /home/opam/src/datakit -n
RUN opam depext i9p && opam install i9p --deps        # Install i9p deps

RUN opam pin add github https://github.com/samoht/ocaml-github.git#status-context

COPY . /home/opam/src/datakit

RUN sudo chown -R opam.nogroup /home/opam/src
WORKDIR /home/opam/src/datakit

RUN opam config exec -- make && make test && make install

EXPOSE 5640

RUN sudo mkdir /data && sudo chown opam.nogroup /data && chmod 700 /data && \
    sudo cp /home/opam/.opam/system/bin/datakit /usr/bin/datakit

CMD ["/usr/bin/datakit", "--url=tcp://0.0.0.0:5640", "--git=/data", "--verbose=debug"]
