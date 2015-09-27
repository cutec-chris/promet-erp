#!/bin/bash
basedir=$(pwd)
cd promet/source/promet.erp
. ../../setup/build-tools/setup_enviroment.sh
echo "Building promet.erp..."
# Build components
$lazbuild prometerp.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
cd $basedir