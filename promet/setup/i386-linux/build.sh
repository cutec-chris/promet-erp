#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-linux
. ../../setup/build-tools/setup_enviroment.sh
echo "Building linux packages..."
# Packages
cd $basedir
cd promet/setup/i386-linux
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`
Date=20$Year$Month$Day
TmpDir=/tmp
BuildDir=$TmpDir/software_build

unzip -d $BuildDir $basedir/promet/setup/output/$BUILD_VERSION/prometerp-$BUILD_VERSION.zip



target=win32tools_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
rm $basedir/promet/setup/output/$BUILD_VERSION/$targetfile
zip -rq $basedir/promet/setup/output/$BUILD_VERSION/$targetfile tools\*.*
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd ..

cd $basedir


