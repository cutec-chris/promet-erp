#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-win32
. ../../setup/build-tools/setup_enviroment.sh
echo "Building win32 stuff..."
# Build components
target=sqliteclient_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip -rq $basedir/promet/setup/output/$targetfile sqlite3.dll
cd $basedir/promet/output
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd $basedir
cd promet/setup/i386-win32

target=mysqlclient_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip -rq $basedir/promet/setup/output/$targetfile libmysql.dll
cd $basedir/promet/output
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd $basedir
cd promet/setup/i386-win32

cd postgres_client
target=postgresclient_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
zip -rq $basedir/promet/setup/output/$targetfile *.dll
cd $basedir/promet/output
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd ..

cd $basedir
cd promet/setup/i386-win32
target=win32tools_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
rm $basedir/promet/setup/output/$targetfile
zip -rq $basedir/promet/setup/output/$targetfile tools\*.* freetype-6.dll
cd $basedir/promet/output
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
fi
cd ..

cd $basedir


