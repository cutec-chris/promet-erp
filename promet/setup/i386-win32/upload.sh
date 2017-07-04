#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-win32
. ../../setup/build-tools/setup_enviroment.sh
echo "Uploading win32 stuff..."
TARGET_CPU=$1
TARGET_OS=$2
cd $basedir
echo "====Portable Installation====" >> /tmp/act_alphadownload.txt
cd $basedir/promet/setup/output
for f in *.paf.exe
do
  echo "Processing $f file..."
  # take action on each file. $f store current file name
  targetfile=$f
  cd $basedir/promet/setup/output
  . ../../setup/build-tools/doupload.sh $targetfile
  cd ..
  cd $basedir
  done
cd $basedir/promet/setup/output
for f in *-portable.zip
do
  echo "Processing $f file..."
  # take action on each file. $f store current file name
  targetfile=$f
  cd $basedir/promet/setup/output
  . ../../setup/build-tools/doupload.sh $targetfile
  cd ..
  cd $basedir
  done

cd $basedir
echo "====Zip Dateien====" >> /tmp/act_alphadownload.txt
echo "Diese Downloads werden normalerweise intern vom Windows Setup oder Updatern intern verwendet. Sie können jedoch auch verwendet werden um sich eine maßgeschneiderte Installation zusammenzubauen. Sie können einfach in ein Verzeichnis entpackt werden und von dort gestartet." >> /tmp/act_alphadownload.txt
cd promet/setup/i386-win32
target=win32tools_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
cd $basedir/promet/setup/output
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd ..

cd $basedir/promet/setup/output
for f in *$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
do
  echo "Processing $f file..."
  # take action on each file. $f store current file name
  targetfile=$f
  targetcur=$target-current.zip
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
  done
cd $basedir

echo "====Bibliotheken====" >> /tmp/act_alphadownload.txt
# Build components
target=sqliteclient_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
cd $basedir/promet/setup/output
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd $basedir
cd promet/setup/i386-win32

target=mysqlclient_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
cd $basedir/promet/setup/output
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd $basedir
cd promet/setup/i386-win32

cd postgres_client
target=postgresclient_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
cd $basedir/promet/setup/output
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd ..
cd $basedir
