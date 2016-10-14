#!/bin/bash
basedir=$(pwd)
cd promet
. setup/build-tools/setup_enviroment.sh
echo "Building importdata..."
# Build components
#set -xv
#copy Files
target=importdata
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip -rq $basedir/promet/setup/output/$targetfile importdata
if [ "$1" = "upload" ]; then
  . setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd $basedir
