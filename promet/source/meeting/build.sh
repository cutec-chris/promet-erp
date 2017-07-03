#!/bin/bash
basedir=$(pwd)
cd promet/source/meeting
. ../../setup/build-tools/setup_enviroment.sh
echo "Building meeting..."
# Build components
$lazbuild meeting.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=meeting_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip $basedir/promet/setup/output/$targetfile meetingminutes$TARGET_EXTENSION
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd $basedir
