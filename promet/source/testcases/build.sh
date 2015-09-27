#!/bin/bash
basedir=$(pwd)
cd promet/source/testcases
. ../../setup/build-tools/setup_enviroment.sh
echo "Building testcases..."
# Build components
$lazbuild consoletest.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
echo "Executing testcases..."
../../output/x86_64-linux/consoletest --mandant=Test
if [ "$?" -ne "0" ]; then
  echo "testcases failed"
  exit 1
fi
echo "."
cd $basedir