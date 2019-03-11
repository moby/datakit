FROM ocaml/opam2:alpine
RUN sudo apk add --no-cache tzdata aspcud gmp-dev perl libev-dev m4

ENV OPAMERRLOGLEN=0 OPAMYES=1

RUN git -C /home/opam/opam-repository fetch origin && \
    git -C /home/opam/opam-repository reset b01a2e4e1ac2fb545d593ad40bc95b4a78dec9e2 --hard && \
    opam update -u

RUN opam install alcotest lwt conf-libev inotify

COPY check-libev.ml /tmp/check-libev.ml
RUN opam config exec -- ocaml /tmp/check-libev.ml

# cache opam install of dependencies

COPY *.opam /home/opam/src/datakit/
RUN  opam pin add /home/opam/src/datakit/ -n

# install dependencies

RUN OPAMSOLVERTIMEOUT=120 opam install datakit-client-git datakit-client-9p --deps -t

COPY . /home/opam/src/datakit
RUN sudo chown opam.nogroup -R /home/opam/src/datakit

RUN opam update --dev && opam upgrade
RUN opam install datakit-client-9p datakit-client-git -tyv

RUN sudo mkdir /data && sudo chown opam.nogroup /data && chmod 700 /data && \
    sudo cp $(opam config exec -- which datakit-mount) /usr/bin/datakit-mount

CMD bash -c "/usr/bin/datakit-mount -h $(getent hosts datakit | awk '{print $1}')"; \
    bash
