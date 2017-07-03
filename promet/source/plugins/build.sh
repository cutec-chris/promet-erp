#!/bin/bash
basedir=$(pwd)
cd promet/source/plugins
. ../../setup/build-tools/setup_enviroment.sh
echo "Building plugins..."
echo $(pwd)
# Build components
echo $lazbuild dwgfile/dwgfile.lpi
$lazbuild dwgfile/dwgfile.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
fi
$lazbuild mindmap/mindmap.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
fi
$lazbuild oofile/oofile.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
fi
$lazbuild solidworks/solidworks.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
fi
$lazbuild target3001/target3001.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
fi
$lazbuild vectorfile/vectorfile.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
fi
$lazbuild winthumb/winthumb.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
fi

#set -xv
#copy Files
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=plugins_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip $basedir/promet/setup/output/$targetfile plugins/*.wlx
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd $basedir
