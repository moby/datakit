#FROM ocaml/opam-dev:alpine-3.5_ocaml-4.04.0
FROM ocaml/opam-dev@sha256:573b5d71e94104590a8282a9ae67b63a6144b15bafdacb9980391c84ca730615
ENV OPAMERRLOGLEN=0 OPAMYES=1
RUN sudo apk add tzdata aspcud

RUN opam pin add -yn protocol-9p.0.8.0 'https://github.com/talex5/ocaml-9p.git#ping'
RUN opam pin add -yn datakit-client.dev https://github.com/docker/datakit.git

ADD datakit-ci.opam /tmp/deps/opam
RUN opam pin add -y tmp /tmp/deps/opam -n
RUN opam depext tmp -y
RUN opam install --deps-only tmp -y

ADD . /home/opam/datakit
RUN sudo chown opam /home/opam/datakit
RUN opam pin add -k git datakit-client.dev /home/opam/datakit -y
RUN opam pin add -k git datakit-ci.dev /home/opam/datakit -y

# Needed if we decide to run the tests
RUN opam pin add -k git datakit-github.dev /home/opam/datakit -yn

VOLUME /secrets
