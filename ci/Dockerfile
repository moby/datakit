FROM ubuntu:latest
RUN apt-get update && apt-get install -y docker.io --no-install-recommends
ADD map.sh ci.sh ./
CMD ./ci.sh
