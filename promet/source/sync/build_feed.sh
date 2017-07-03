#!/bin/bash
basedir=$(pwd)
cd promet/source/sync
. ../../setup/build-tools/setup_enviroment.sh
echo "Building feed components..."
# Build components
$lazbuild feedreceiver.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  $lazbuild feedreceiver.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
fi
if [ $? -ne 0 ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild twitterreceiver.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  $lazbuild twitterreceiver.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
fi
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
#  twitterreceiver dont buildable with old fpc
#  exit 1
fi
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=feedreceiver_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip $basedir/promet/setup/output/$targetfile feedreceiver$TARGET_EXTENSION twitterreceiver$TARGET_EXTENSION
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd $basedir
