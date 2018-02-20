#!/bin/bash
basedir=$(pwd)
cd promet/source/tools
. ../../setup/build-tools/setup_enviroment.sh
echo "Building visual tools..."
# Build components
$lazbuild -q ../components/powerpdf/pack_powerpdf.lpk
$lazbuild -q ../components/fpreport/fpreport_fcl.lpk
$lazbuild -q ../base/pvisualprometapp.lpk
$lazbuild wizardmandant.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  $lazbuild wizardmandant.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
fi
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild pstarter.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  $lazbuild pstarter.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
fi
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild helpviewer.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  $lazbuild helpviewer.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
fi
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=visualtools_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip $basedir/promet/setup/output/$targetfile pstarter$TARGET_EXTENSION helpviewer$TARGET_EXTENSION
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd $basedir
