#!/bin/bash
. ./promet/setup/build-tools/setup_enviroment.sh
cd /tmp
rm -R *
git clone https://gitlab-ci-token:$CI_BUILD_TOKEN@192.168.177.120:10443/promet/promet-client-docker.git
cd promet-client-docker
cat ./Dockerfile.template | sed -e "s/VERSION/$BUILD_VERSION/g" > ./Dockerfile
git commit -a -m "Dockerfile with Version updated"
git push origin master