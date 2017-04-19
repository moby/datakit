#FROM ocaml/opam-dev:alpine-3.5_ocaml-4.04.0
FROM ocaml/opam-dev@sha256:573b5d71e94104590a8282a9ae67b63a6144b15bafdacb9980391c84ca730615
ENV OPAMERRLOGLEN=0 OPAMYES=1
RUN sudo apk add tzdata aspcud

# Hack: help the opam solver out a bit...
RUN opam pin add cmdliner 0.9.8

RUN opam depext -ui lwt inotify alcotest conf-libev lambda-term

RUN opam pin add -yn protocol-9p.0.8.0 'https://github.com/talex5/ocaml-9p.git#ping'

# cache opam install of dependencies
COPY datakit-client.opam /home/opam/src/datakit/datakit-client.opam
COPY datakit-server.opam /home/opam/src/datakit/datakit-server.opam
RUN opam pin add datakit-server.dev /home/opam/src/datakit -yn
RUN opam pin add datakit-client.dev /home/opam/src/datakit -yn
RUN opam depext datakit-server && opam install datakit-server --deps

COPY . /home/opam/src/datakit
RUN sudo chown opam.nogroup -R /home/opam/src/datakit
RUN cd /home/opam/src/datakit && \
    git diff && git status --porcelain && \
    git checkout . && scripts/watermark.sh && \
    git status --porcelain

RUN opam update datakit-server
RUN opam install datakit-server.dev -vv

COPY opam /home/opam/src/datakit/opam
RUN opam pin add datakit.dev /home/opam/src/datakit -n
RUN opam pin add datakit-github.dev /home/opam/src/datakit -n
RUN opam depext datakit && opam install datakit -y --deps

COPY . /home/opam/src/datakit/
RUN sudo chown opam.nogroup -R /home/opam/src/datakit
RUN cd /home/opam/src/datakit && \
    git diff && git status --porcelain && \
    git checkout . && scripts/watermark.sh && \
    git status --porcelain

RUN opam update datakit
RUN opam install datakit -vv -y

EXPOSE 5640

RUN sudo mkdir /data && sudo chown opam.nogroup /data && chmod 700 /data && \
    sudo cp $(opam config exec -- which datakit) /usr/bin/datakit && \
    sudo cp $(opam config exec -- which datakit-mount) /usr/bin/datakit-mount

RUN opam config exec -- ocaml /home/opam/src/datakit/check-libev.ml

ENTRYPOINT ["/usr/bin/datakit"]
CMD ["--url=tcp://0.0.0.0:5640", "--git=/data", "-v"]
