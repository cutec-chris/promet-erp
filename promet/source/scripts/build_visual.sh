#!/bin/bash
basedir=$(pwd)
cd promet/source/statistics
. ../../setup/build-tools/setup_enviroment.sh
echo "Building script edit..."
# Build components
$lazbuild pscriptedit.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=pscriptedit_$TARGET_CPU-$TARGET_OS-$BUILD
targetfile=$target_VERSION.zip
zip $basedir/promet/setup/output/$BUILD_VERSION/$targetfile pscriptedit$TARGET_EXTENSION
. ../../setup/build-tools/doupload.sh $targetfile $target_current.zip
cd $basedir
