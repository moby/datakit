#FROM ocaml/opam-dev:alpine-3.5_ocaml-4.04.0
FROM ocaml/opam-dev@sha256:573b5d71e94104590a8282a9ae67b63a6144b15bafdacb9980391c84ca730615
ENV OPAMERRLOGLEN=0 OPAMYES=1
RUN sudo apk add tzdata aspcud gmp-dev perl

RUN opam pin add -yn protocol-9p.0.8.0 'https://github.com/talex5/ocaml-9p.git#ping'
RUN opam depext -ui lwt inotify alcotest conf-libev lambda-term asl win-eventlog camlzip irmin-watcher mtime mirage-flow conduit hvsock cohttp prometheus-app git.1.9.3 mirage-tc irmin.0.12.0 cmdliner.0.9.8 protocol-9p channel rresult win-error named-pipe

COPY check-libev.ml /tmp/check-libev.ml
RUN opam config exec -- ocaml /tmp/check-libev.ml

# cache opam install of dependencies
COPY datakit.opam /home/opam/src/datakit/
COPY datakit-client.opam /home/opam/src/datakit/datakit-client.opam
COPY datakit-server.opam /home/opam/src/datakit/datakit-server.opam
COPY datakit-github.opam /home/opam/src/datakit/datakit-github.opam
RUN opam pin add datakit-server.dev /home/opam/src/datakit -yn && \
    opam pin add datakit-client.dev /home/opam/src/datakit -yn && \
    opam pin add datakit.dev /home/opam/src/datakit -yn        && \
    opam pin add datakit-github.dev /home/opam/src/datakit -yn

RUN opam depext -y datakit
RUN opam install -y --deps-only datakit-client datakit-server

COPY . /home/opam/src/datakit

RUN sudo chown opam.nogroup -R /home/opam/src/datakit
RUN cd /home/opam/src/datakit && \
    git diff && git status --porcelain && \
    git checkout . && scripts/watermark.sh && \
    git status --porcelain

RUN opam install datakit -ytv

RUN sudo cp $(opam config exec -- which datakit) /usr/bin/datakit && \
    sudo cp $(opam config exec -- which datakit-mount) /usr/bin/datakit-mount

FROM alpine:3.5
RUN apk add --no-cache libev gmp tzdata ca-certificates git openssh-client bash
EXPOSE 5640
ENTRYPOINT ["/usr/bin/datakit"]
CMD ["--url=tcp://0.0.0.0:5640", "--git=/data", "-v"]
COPY --from=0 /usr/bin/datakit /usr/bin/datakit
COPY --from=0 /usr/bin/datakit-mount /usr/bin/datakit-mount
