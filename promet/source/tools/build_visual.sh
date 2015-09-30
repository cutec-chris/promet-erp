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
cd $basedir
#set -xv
#copy Files
cd $basedir
rm -r $BUILD_DIR
mkdir $BUILD_DIR
mkdir $BUILD_DIR/tools
cp promet/output/$TARGET_CPU-$TARGET_OS/wizardmandant$TARGET_EXTENSION $BUILD_DIR
cp promet/output/$TARGET_CPU-$TARGET_OS/pstarter$TARGET_EXTENSION $BUILD_DIR
cp promet/output/$TARGET_CPU-$TARGET_OS/helpviewer$TARGET_EXTENSION $BUILD_DIR
cp promet/output/$TARGET_CPU-$TARGET_OS/tableedit$TARGET_EXTENSION $BUILD_DIR/tools
cd $BUILD_DIR
zip -rq $basedir/promet/setup/output/$BUILD_VERSION/visualtools_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip .
cd $basedir