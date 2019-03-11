FROM ocaml/opam2:alpine
RUN sudo apk add --no-cache tzdata aspcud gmp-dev perl libev-dev m4

ENV OPAMERRLOGLEN=0 OPAMYES=1

RUN git -C /home/opam/opam-repository fetch origin && \
    git -C /home/opam/opam-repository reset b01a2e4e1ac2fb545d593ad40bc95b4a78dec9e2 --hard && \
    opam update -u

RUN opam install alcotest lwt conf-libev inotify

COPY check-libev.ml /tmp/check-libev.ml
RUN opam exec -- ocaml /tmp/check-libev.ml

# cache opam install of dependencies

COPY *.opam /home/opam/src/datakit/
RUN  opam pin add /home/opam/src/datakit/ -n

# install dependencies

RUN sudo apk add --no-cache libressl-dev linux-headers pcre-dev
RUN opam install datakit-ci --deps -t

# copy local sources

COPY . /home/opam/src/datakit
RUN sudo chown opam.nogroup -R /home/opam/src/datakit

RUN opam update --dev && opam upgrade

RUN opam install datakit-ci -ytv

VOLUME /secrets
