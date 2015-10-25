#!/bin/bash
basedir=$(pwd)
cd promet/source/plugins
. ../../setup/build-tools/setup_enviroment.sh
echo "Building plugins..."
# Build components
$lazbuild dwgfile/dwgfile.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
fi
$lazbuild mindmap/mindmap.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
fi
$lazbuild oofile/oofile.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
fi
$lazbuild solidworks/solidworks.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
fi
$lazbuild target3001/target3001.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
fi
$lazbuild vectorfile/vectorfile.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
fi
$lazbuild winthumb/winthumb.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
fi

#set -xv
#copy Files
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=plugins_$TARGET_CPU-$TARGET_OS-$BUILD
targetfile=$target_VERSION.zip
zip $basedir/promet/setup/output/$BUILD_VERSION/$targetfile plugins/*.wlx
. ../../setup/build-tools/doupload.sh $targetfile $target_current.zip
cd $basedir
