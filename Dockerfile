FROM ocaml/opam:alpine

RUN sudo apk add ncurses-dev libev-dev
RUN opam depext lwt && opam install lwt alcotest conf-libev

# cache opam install of dependencies
COPY opam /home/opam/src/datakit/opam
RUN opam pin add datakit.dev -k git /home/opam/src/datakit#HEAD -n
RUN opam depext datakit && opam install datakit --deps

COPY . /home/opam/src/datakit

RUN opam install datakit.dev -vv

EXPOSE 5640

RUN sudo mkdir /data && sudo chown opam.nogroup /data && chmod 700 /data && \
    sudo cp $(opam config exec -- which datakit) /usr/bin/datakit && \
    sudo cp $(opam config exec -- which datakit-mount) /usr/bin/datakit-mount

RUN opam config exec -- ocaml /home/opam/src/datakit/check-libev.ml

ENTRYPOINT ["/usr/bin/datakit"]
CMD ["--url=tcp://0.0.0.0:5640", "--git=/data", "-v"]
