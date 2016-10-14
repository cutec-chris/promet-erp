#!/bin/bash
basedir=$(pwd)
cd promet/source/scripts/plugins
. ../../setup/build-tools/setup_enviroment.sh
echo "Building script plugins..."
# Build components
$lazbuild audio/audio.lpi $BUILD_ARCH $BUILD_PARAMS > build.txt
$lazbuild dxfwrite/dxfwrite.lpi $BUILD_ARCH $BUILD_PARAMS >> build.txt
$lazbuild hexfiles/hexfiles.lpi $BUILD_ARCH $BUILD_PARAMS >> build.txt
$lazbuild net/net.lpi $BUILD_ARCH $BUILD_PARAMS >> build.txt
$lazbuild serialport/serialport.lpi $BUILD_ARCH $BUILD_PARAMS >> build.txt
$lazbuild tinkerforge/tinkerforge.lpi $BUILD_ARCH $BUILD_PARAMS >> build.txt
$lazbuild usb/usb.lpi $BUILD_ARCH $BUILD_PARAMS >> build.txt
$lazbuild video/video.lpi $BUILD_ARCH $BUILD_PARAMS >> build.txt

cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
#target=pscript_$TARGET_CPU-$TARGET_OS
#targetfile=$target-$BUILD_VERSION.zip
#targetcur=$target-current.zip
#zip $basedir/promet/setup/output/$targetfile pscript$TARGET_EXTENSION
#if [ "$1" = "upload" ]; then
#  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
#fi
cd $basedir
