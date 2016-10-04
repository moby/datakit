FROM docker/datakit:server

COPY opam /home/opam/src/datakit/opam
RUN opam pin add datakit.dev /home/opam/src/datakit -n
RUN opam pin add git --dev -n
RUN opam depext datakit && opam install datakit --deps

COPY . /home/opam/src/datakit/
RUN sudo chown opam.nogroup -R /home/opam/src/datakit
RUN cd /home/opam/src/datakit && \
    git diff && git status --porcelain && \
    git checkout . && scripts/watermark.sh && \
    git status --porcelain

RUN opam update datakit
RUN opam install datakit -vv

EXPOSE 5640

RUN sudo mkdir /data && sudo chown opam.nogroup /data && chmod 700 /data && \
    sudo cp $(opam config exec -- which datakit) /usr/bin/datakit && \
    sudo cp $(opam config exec -- which datakit-mount) /usr/bin/datakit-mount

RUN opam config exec -- ocaml /home/opam/src/datakit/check-libev.ml

ENTRYPOINT ["/usr/bin/datakit"]
CMD ["--url=tcp://0.0.0.0:5640", "--git=/data", "-v"]
