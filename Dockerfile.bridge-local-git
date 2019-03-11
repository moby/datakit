FROM datakit/client

COPY . /home/opam/src/datakit
RUN sudo chown opam.nogroup -R /home/opam/src/datakit

RUN opam update --dev && opam upgrade

RUN opam install datakit-bridge-local-git -tyv
RUN sudo cp $(opam config exec -- which datakit-bridge-local-git) /usr/bin/

FROM alpine:3.9
RUN apk add --no-cache libev gmp tzdata ca-certificates
ENTRYPOINT ["/usr/bin/datakit-bridge-local-git"]
COPY --from=0 /usr/bin/datakit-bridge-local-git /usr/bin/datakit-bridge-local-git
