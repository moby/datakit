#!/bin/bash -eux
HOST=ucp.local
SSH_LOGIN=root@$HOST
docker build -t $HOST/admin/datakit-ci .
docker push $HOST/admin/datakit-ci
ssh $SSH_LOGIN docker pull $HOST/admin/datakit-ci
ssh $SSH_LOGIN docker rm -f datakit-ci
ssh $SSH_LOGIN docker run -d --name datakit-ci -v /srv/datakit:/srv/datakit -v /var/run/docker.sock:/var/run/docker.sock $HOST/admin/datakit-ci
ssh $SSH_LOGIN docker attach --sig-proxy=false datakit-ci
