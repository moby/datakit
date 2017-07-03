FROM ocaml/opam-dev@sha256:1dd4440b3e5f182f705cb5a74f9d4e860c2842b45ed72c199de89a894d13f522
#FROM ocaml/opam-dev:alpine-3.5_ocaml-4.04.0

RUN git -C /home/opam/opam-repository fetch origin && \
    git -C /home/opam/opam-repository reset 482bd5f55 --hard && \
    opam update -u

ENV OPAMERRLOGLEN=0 OPAMYES=1
RUN sudo apk add tzdata aspcud gmp-dev perl

RUN opam depext -ui lwt inotify alcotest conf-libev asl win-eventlog \
    irmin-watcher mtime mirage-flow conduit hvsock prometheus-app git irmin \
    cmdliner protocol-9p rresult win-error named-pipe

COPY check-libev.ml /tmp/check-libev.ml
RUN opam config exec -- ocaml /tmp/check-libev.ml

# cache opam install of dependencies
COPY datakit.opam /home/opam/src/datakit/
COPY datakit-client.opam /home/opam/src/datakit/datakit-client.opam
COPY datakit-client-9p.opam /home/opam/src/datakit/datakit-client-9p.opam
COPY datakit-server.opam /home/opam/src/datakit/datakit-server.opam
COPY datakit-server-9p.opam /home/opam/src/datakit/datakit-server-9p.opam
RUN opam pin add datakit-server.dev /home/opam/src/datakit -yn && \
    opam pin add datakit-server-9p.dev /home/opam/src/datakit -yn && \
    opam pin add datakit-client.dev /home/opam/src/datakit -yn && \
    opam pin add datakit-client-9p.dev /home/opam/src/datakit -yn && \
    opam pin add datakit.dev /home/opam/src/datakit -yn

RUN opam depext -y datakit
RUN opam install -y --deps-only datakit-client datakit-server
RUN opam install alcotest

COPY . /home/opam/src/datakit

RUN sudo chown opam.nogroup -R /home/opam/src/datakit
RUN cd /home/opam/src/datakit && \
    scripts/watermark.sh && \
    git status --porcelain

# FIXME: warkaround a bug in opam2
RUN opam install datakit-client-9p
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
