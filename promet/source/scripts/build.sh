#!/bin/bash
basedir=$(pwd)
cd promet/source/scripts
. ../../setup/build-tools/setup_enviroment.sh
echo "Building script tools..."
# Build components
$lazbuild pscript.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  $lazbuild pscript.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
fi
if [ $? -ne 0 ]; then
  echo "build failed"
  tail -n 10 build.txt
  exit 1
fi
cd plugins
bash build.sh
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
target=pscript_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip $basedir/promet/setup/output/$targetfile pscript$TARGET_EXTENSION scriptplugins/*.so scriptplugins/*.dll
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd $basedir
