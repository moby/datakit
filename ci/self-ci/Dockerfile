FROM docker/datakit:ci
RUN sudo apk update && sudo apk add docker

ADD . /home/opam/build
WORKDIR /home/opam/build
RUN sudo chown opam .
RUN opam config exec make selfCI && sudo cp _build/selfCI.native /bin/datakit-ci && rm -rf /home/opam/build
USER root
ENTRYPOINT ["/bin/datakit-ci"]
CMD []
