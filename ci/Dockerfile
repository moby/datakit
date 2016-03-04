FROM ubuntu:latest
RUN apt-get update && apt-get install -y docker.io --no-install-recommends
VOLUME /mnt/datakit:/db
ADD ci.sh build.sh ./
CMD ./ci.sh
