FROM docker/datakit:ci
RUN sudo apk add --no-cache libev docker gmp
ADD . /home/opam/build
WORKDIR /home/opam/build
RUN sudo chown opam .
RUN opam config exec make selfCI
ENTRYPOINT ["/home/opam/build/_build/selfCI.native"]
USER root
