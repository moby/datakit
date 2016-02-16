FROM datakit-sdk
COPY src /home/opam/src/datakit-static
RUN opam pin add -n jsont.dev https://github.com/dbuenzli/jsont.git
RUN opam pin add -n opam-lib --dev
RUN opam pin add datakit-static.dev /home/opam/src/datakit-static
RUN sudo chown -R opam.nogroup /home/opam/src
WORKDIR /home/opam/src/datakit-static
RUN opam config exec make
