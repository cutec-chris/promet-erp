#!/bin/bash
basedir=$(pwd)
cd promet/source/tools
. ../../setup/build-tools/setup_enviroment.sh
echo "Building visual tools..."
# Build components
$lazbuild wizardmandant.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild pstarter.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild helpviewer.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild tableedit.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=visualtools_$TARGET_CPU-$TARGET_OS-$BUILD
targetfile=$target_VERSION.zip
zip $basedir/promet/setup/output/$BUILD_VERSION/$targetfile wizardmandant$TARGET_EXTENSION pstarter$TARGET_EXTENSION helpviewer$TARGET_EXTENSION tableedit$TARGET_EXTENSION
. ../../setup/build-tools/doupload.sh $targetfile $target_current.zip
cd $basedir
