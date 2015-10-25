#!/bin/bash
basedir=$(pwd)
cd promet/source/statistics
. ../../setup/build-tools/setup_enviroment.sh
echo "Building statistics..."
# Build components
$lazbuild statistics.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=timeregistering_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip $basedir/promet/setup/output/$BUILD_VERSION/$targetfile timeregistering$TARGET_EXTENSION
. ../../setup/build-tools/doupload.sh $targetfile $targetcur &
cd $basedir
