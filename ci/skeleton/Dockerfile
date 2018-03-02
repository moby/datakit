FROM datakit/ci

ARG CONFIG=exampleCI
ADD . /datakit-ci
WORKDIR /datakit-ci
RUN sudo chown opam .
RUN opam config exec make $CONFIG && ln _build/$CONFIG.native /datakit-ci/datakit-ci && rm -rf _build
USER root
ENTRYPOINT ["/datakit-ci/datakit-ci"]
CMD []
