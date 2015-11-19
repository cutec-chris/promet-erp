#!/bin/bash
basedir=$(pwd)
cd promet/source/tools
. ../../setup/build-tools/setup_enviroment.sh
echo "Building archivestore..."
# Build components
$lazbuild archivestore.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=archivestore_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip $basedir/promet/setup/output/$BUILD_VERSION/$targetfile archivestore$TARGET_EXTENSION
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd $basedir
