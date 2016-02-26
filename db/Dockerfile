FROM datakit-sdk
COPY src /home/opam/src/db
RUN opam pin add db.dev /home/opam/src/db
RUN sudo chown -R opam.nogroup /home/opam/src
WORKDIR /home/opam/src/db
RUN opam config exec make
ENTRYPOINT ["/home/opam/src/db/db.native"]
