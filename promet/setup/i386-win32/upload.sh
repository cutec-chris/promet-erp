#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-win32
. ../../setup/build-tools/setup_enviroment.sh
echo "Uploading win32 stuff..."
TARGET_CPU=$1
TARGET_OS=$2
# Build components
target=sqliteclient_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd $basedir
cd promet/setup/i386-win32

target=mysqlclient_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd $basedir
cd promet/setup/i386-win32

cd postgres_client
target=postgresclient_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd ..

cd $basedir
cd promet/setup/i386-win32
target=win32tools_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
cd $basedir/promet/output/$TARGET_CPU-$TARGET_OS
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd ..

cd $basedir


