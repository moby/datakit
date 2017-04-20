FROM datakit/ci AS build-env
RUN sudo apk add --no-cache libev docker gmp
ADD . /home/opam/build
WORKDIR /home/opam/build
RUN sudo chown opam .
RUN opam config exec make selfCI
RUN sudo cp selfCI.native /usr/local/bin/ci

FROM alpine:3.5
RUN apk add --no-cache libev docker gmp tzdata ca-certificates

USER root
ENTRYPOINT ["/usr/local/bin/ci"]
COPY --from=build-env /usr/local/bin/ci /usr/local/bin/ci
