FROM docker/datakit:ci
RUN sudo apk add docker

ADD . /datakit-ci
WORKDIR /datakit-ci
RUN sudo chown opam .
RUN opam config exec make selfCI && ln _build/selfCI.native /datakit-ci/datakit-ci && rm -rf _build
USER root
ENTRYPOINT ["/datakit-ci/datakit-ci"]
CMD []
