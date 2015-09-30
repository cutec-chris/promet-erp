#!/bin/bash
basedir=$(pwd)
cd promet/importdata
. ../setup/build-tools/setup_enviroment.sh
echo "Building importdata..."
# Build components
#set -xv
#copy Files
zip -rq $basedir/promet/setup/output/$BUILD_VERSION/importdata_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip importdata
cd $basedir