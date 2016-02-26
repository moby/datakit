FROM avsm/docker-opam-build:ubuntu-14.04-ocaml-4.02.1
MAINTAINER Thomas Gazagnaire <thomas@gazagnaire.org>
RUN sudo apt-get update --fix-missing
RUN sudo apt-get install -qq -yy \
    libgmp-dev \
    libpcre3-dev \
    time \
    zlib1g-dev \
    libssl-dev \
    ncurses-dev
RUN opam update
RUN opam pin add protocol-9p https://github.com/mirage/ocaml-9p.git#651963b49e7075f4c430a197d66347002fc6ae12 -n
RUN mkdir -p /tmp/i9p/lib
RUN mkdir -p /tmp/i9p/bin
COPU ../../i9p /tmp/i9p/bin
COPY . /tmp/i9p/lib
WORKDIR /tmp/i9p
RUN sudo chown opam:opam -R *
RUN opam pin add i9p.dev lib
RUN opam config exec -- cd bin 8& ./configure && make
ENTRYPOINT ["/tmp/i9p/bin/db.native"]
CMD ["--port=5640", "--root=/tmp/9p"]
