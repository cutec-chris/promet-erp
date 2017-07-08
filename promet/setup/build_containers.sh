#!/bin/bash
. ./promet/setup/build-tools/setup_enviroment.sh

cd /tmp

build_conainer()
{
  rm -r -f $1
  git clone git@github.com:cutec-chris/$1.git
  cd $1
  cat ./Dockerfile.template | sed -e "s/PVERSION/$BUILD_VERSION/g" | sed -e "s/RANDOMNUMBER/$RANDOM/g" > Dockerfile
  git commit -a -m "Dockerfile Updated for new Build with Version $BUILD_VERSION"
  git push origin master
  cd ..
}

build_conainer promet-client-docker;
build_conainer promet-server-docker;
build_conainer promet-server-alpine-docker;

