#!/bin/bash
basedir=$(pwd)
cd promet/source/webservers
. ../../setup/build-tools/setup_enviroment.sh
echo "Building imapserver..."
# Build components
$lazbuild imapserver.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
zip $basedir/promet/setup/output/$BUILD_VERSION/imapserver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip tools/imapserver$TARGET_EXTENSION
cd $basedir
