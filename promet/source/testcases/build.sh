#!/bin/bash
basedir=$(pwd)
cd promet/source/testcases
. ../../setup/build-tools/setup_enviroment.sh
echo "Building testcases..."
# Build components
$lazbuild consoletest.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  $lazbuild consoletest.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
fi
if [ "$?" -ne "0" ]; then
  $lazbuild consoletest.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
fi
if [ "$?" -ne "0" ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
echo "Executing testcases..."
../../output/$TARGET_CPU-$TARGET_OS/consoletest --mandant=help --config-path=../../help/config
if [ "$?" = "0" ]; then
  ../../output/$TARGET_CPU-$TARGET_OS/consoletest --mandant=help --config-path=../../help/config
fi
if [ "$?" = "0" ]; then
  echo "."
  echo "testcases failed"
  exit 1
fi
echo "."
cd $basedir
