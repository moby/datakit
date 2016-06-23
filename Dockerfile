FROM ocaml/opam:alpine

# Workaround for https://github.com/docker/datakit/issues/152
RUN cd /home/opam/opam-repository && git reset --hard dd38f0690ff01c8e1ed0eed231307ba4d00c50df
RUN opam depext lwt ssl &&  opam install lwt alcotest oasis
RUN opam pin add asl.dev https://github.com/mirage/ocaml-asl.git
#RUN cd /home/opam/opam-repository && git pull && opam update

RUN opam pin add hvsock https://github.com/djs55/ocaml-hvsock.git

COPY opam /home/opam/src/datakit/opam
RUN opam pin add datakit.dev /home/opam/src/datakit -n
RUN opam depext datakit && opam install datakit --deps # Install datakit deps

COPY . /home/opam/src/datakit/
RUN sudo chown -R opam.nogroup /home/opam/src
WORKDIR /home/opam/src/datakit

RUN opam config exec -- make && make test && make install

EXPOSE 5640

RUN sudo mkdir /data && sudo chown opam.nogroup /data && chmod 700 /data && \
    sudo cp /home/opam/.opam/system/bin/datakit /usr/bin/datakit

ENTRYPOINT ["/usr/bin/datakit"]
CMD ["--url=tcp://0.0.0.0:5640", "--git=/data", "-vv"]
