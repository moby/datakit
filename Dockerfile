FROM ocaml/opam2:alpine
RUN sudo apk add --no-cache tzdata aspcud gmp-dev perl libev-dev m4

ENV OPAMERRLOGLEN=0 OPAMYES=1

RUN git -C /home/opam/opam-repository fetch origin && \
    git -C /home/opam/opam-repository reset df84483e523f1eec710a2118e05920f9cdf882d3 --hard && \
    opam update -u

RUN opam install alcotest lwt conf-libev inotify

COPY check-libev.ml /tmp/check-libev.ml
RUN opam exec -- ocaml /tmp/check-libev.ml

# cache opam install of dependencies

COPY *.opam /home/opam/src/datakit/
RUN  opam pin add /home/opam/src/datakit/ -n

# install dependencies

RUN opam install datakit --deps -t

# copy local sources

COPY . /home/opam/src/datakit
RUN sudo chown opam.nogroup -R /home/opam/src/datakit

RUN opam update --dev && opam upgrade

RUN opam install datakit -ytv

RUN sudo cp $(opam config exec -- which datakit) /usr/bin/datakit && \
    sudo cp $(opam config exec -- which datakit-mount) /usr/bin/datakit-mount

FROM alpine:3.9
RUN apk add --no-cache libev gmp tzdata ca-certificates git openssh-client bash
EXPOSE 5640
ENTRYPOINT ["/usr/bin/datakit"]
CMD ["--url=tcp://0.0.0.0:5640", "--git=/data", "-v"]
COPY --from=0 /usr/bin/datakit /usr/bin/datakit
COPY --from=0 /usr/bin/datakit-mount /usr/bin/datakit-mount
