#!/bin/bash
basedir=$(pwd)
cd promet/source/tools
. ../../setup/build-tools/setup_enviroment.sh
echo "Building tools..."
$lazbuild cmdwizardmandant.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild checkin.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild checkout.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild processdaemon.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild ../webservers/pappserver.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=tools_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip $basedir/promet/setup/output/$BUILD_VERSION/$targetfile checkin$TARGET_EXTENSION checkout$TARGET_EXTENSION processdaemon$TARGET_EXTENSION pappserver$TARGET_EXTENSION
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd $basedir
