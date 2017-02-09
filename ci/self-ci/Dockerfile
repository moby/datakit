FROM docker/datakit:ci
ADD . /home/opam/build
WORKDIR /home/opam/build
RUN sudo chown opam .
RUN opam config exec make selfCI
