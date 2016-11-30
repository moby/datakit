FROM docker/datakit:ci
RUN sudo apk add docker
RUN opam pin add protocol-9p.0.7.4 https://github.com/mirage/ocaml-9p.git#v0.7.4
RUN opam pin add datakit-ci https://github.com/talex5/datakit.git#snap19

ADD . /datakit-ci
WORKDIR /datakit-ci
RUN sudo chown opam .
RUN opam config exec make selfCI && ln _build/selfCI.native /datakit-ci/datakit-ci && rm -rf _build
USER root
ENTRYPOINT ["/datakit-ci/datakit-ci"]
CMD []
