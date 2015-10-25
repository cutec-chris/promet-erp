#!/bin/bash
basedir=$(pwd)
cd promet/source/promet.erp
. ../../setup/build-tools/setup_enviroment.sh
echo "Building promet.erp..."
# Build components
$lazbuild prometerp.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=prometerp_$TARGET_CPU-$TARGET_OS-$BUILD
targetfile=$target_VERSION.zip
zip $basedir/promet/setup/output/$BUILD_VERSION/$targetfile prometerp$TARGET_EXTENSION
. ../../setup/build-tools/doupload.sh $targetfile $target_current.zip
cd $basedir
