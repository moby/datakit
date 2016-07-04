FROM ocaml/opam:alpine

RUN sudo apk add ncurses-dev libev-dev
RUN opam depext lwt && opam install lwt alcotest conf-libev

# cache opam install of dependencies
COPY opam /home/opam/src/datakit/opam
RUN opam pin add datakit.dev /home/opam/src/datakit -n
RUN opam depext datakit && opam install datakit --deps

COPY . /home/opam/src/datakit

RUN opam install datakit

EXPOSE 5640

RUN sudo mkdir /data && sudo chown opam.nogroup /data && chmod 700 /data && \
    sudo cp /home/opam/.opam/system/bin/datakit /usr/bin/datakit && \
    sudo cp /home/opam/.opam/system/bin/datakit-mount /usr/bin/datakit-mount

RUN opam config exec -- ocaml /home/opam/src/datakit/check-libev.ml

ENTRYPOINT ["/usr/bin/datakit"]
CMD ["--url=tcp://0.0.0.0:5640", "--git=/data", "-vv"]
