#!/bin/bash
basedir=$(pwd)
cd promet/source/tools
. ../../setup/build-tools/setup_enviroment.sh
echo "Building tools..."
# Build components
$lazbuild cmdwizardmandant.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild checkin.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild checkout.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild processdaemon.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild processmanager.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
#set -xv
#copy Files
cd $basedir
rm -r $BUILD_DIR
mkdir $BUILD_DIR
mkdir $BUILD_DIR/tools
cp promet/output/$TARGET_CPU-$TARGET_OS/cmdwizardmandant$TARGET_EXTENSION $BUILD_DIR
cp promet/output/$TARGET_CPU-$TARGET_OS/checkin$TARGET_EXTENSION $BUILD_DIR
cp promet/output/$TARGET_CPU-$TARGET_OS/checkout$TARGET_EXTENSION $BUILD_DIR
cp promet/output/$TARGET_CPU-$TARGET_OS/tools/processdaemon$TARGET_EXTENSION $BUILD_DIR/tools
cp promet/output/$TARGET_CPU-$TARGET_OS/tools/processmanager$TARGET_EXTENSION $BUILD_DIR/tools
cd $BUILD_DIR
zip -rq $basedir/promet/setup/output/$BUILD_VERSION/tools_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip .
cd $basedir