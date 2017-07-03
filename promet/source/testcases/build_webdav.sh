#!/bin/bash
basedir=$(pwd)
cd promet/source/testcases
. ../../setup/build-tools/setup_enviroment.sh
echo "Building testcases..."
# Build components
$lazbuild consoletest_webdav.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  $lazbuild consoletest_webdav.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
fi
if [ $? -ne 0 ]; then
  $lazbuild consoletest_webdav.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
fi
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
cd $basedir
echo "Executing testcases..."
./promet/output/$TARGET_CPU-$TARGET_OS/consoletest_webdav --mandant=help --config-path=./promet/help/config