#!/bin/bash
basedir=$(pwd)
cd promet
. setup/build-tools/setup_enviroment.sh
echo "Building importdata..."
# Build components
#set -xv
#copy Files
target=help
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip -rq $basedir/promet/setup/output/$BUILD_VERSION/$targetfile importdata
. ../setup/build-tools/doupload.sh $targetfile $targetcur &
cd $basedir