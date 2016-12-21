#!/bin/sh
set -eux
certbot certonly -t --standalone --standalone-supported-challenges http-01 -d datakit.datakit.ci
cp /etc/letsencrypt/live/datakit.datakit.ci/fullchain.pem /secrets/server.crt
cp /etc/letsencrypt/live/datakit.datakit.ci/privkey.pem /secrets/server.key
echo "Now restart the service"
