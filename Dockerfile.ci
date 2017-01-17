FROM ocaml/opam:alpine-3.4_ocaml-4.03.0
RUN sudo apk add tzdata

# TMP
RUN cd /home/opam/opam-repository && \
    git pull && opam update -y

RUN opam depext -ui cohttp yojson ssl tyxml fmt rresult logs lwt conf-libev react camlp4 \
    tls pbkdf ppx_sexp_conv webmachine session asetmap

RUN opam pin add prometheus https://github.com/talex5/datakit.git#prometheus
RUN opam pin add prometheus-app https://github.com/talex5/datakit.git#prometheus
RUN opam pin add datakit-client https://github.com/docker/datakit.git
RUN opam pin add session "https://github.com/talex5/ocaml-session.git#redis"
RUN opam pin add github --dev
RUN opam pin add datakit-github https://github.com/docker/datakit.git

ADD . /home/opam/datakit
RUN opam pin add -k git datakit-ci /home/opam/datakit

VOLUME /secrets
