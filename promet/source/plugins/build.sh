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
  exit 1
fi
$lazbuild mindmap/mindmap.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild oofile/oofile.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild solidworks/solidworks.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild target3001/target3001.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild vectorfile/vectorfile.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild winthumb/winthumb.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed"
  $grep -w "Error:" build.txt
  exit 1
fi

#copy Files
cd $basedir
rm -r $BUILD_DIR
mkdir $BUILD_DIR
mkdir $BUILD_DIR/plugins
cp $basedir/promet/output/$TARGET_CPU-$TARGET_OS/plugins/*.wlx $BUILD_DIR/plugins
cd $BUILD_DIR
zip -rq plugins_$BUILD_VERSION_$TARGET_CPU-$TARGET_OS.zip .
cd $basedir
