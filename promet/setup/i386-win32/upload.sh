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
echo "====Zip Dateien====" >> ./promet/setup/output/act_alphadownload.txt
echo "Diese Downloads werden normalerweise intern vom Windows Setup oder Updatern intern verwendet. Sie können jedoch auch verwendet werden um sich eine maßgeschneiderte Installation zusammenzubauen Sie können einfach in ein Verzeichnis entpackt werden und von dort gestartet." >> ./promet/setup/output/act_alphadownload.txt
cd promet/setup/i386-win32
target=win32tools_$TARGET_CPU-$TARGET_OS
targetfile=$target-$BUILD_VERSION.zip
targetcur=$target-current.zip
cd $basedir/promet/setup/output
. ../../setup/build-tools/doupload.sh $targetfile $targetcur
cd ..

for f in *.zip
do
  echo "Processing $f file..."
  # take action on each file. $f store current file name
  cd promet/setup/i386-win32
  targetfile=$f
  targetcur=$target-current.zip
  cd $basedir/promet/setup/output
  . ../../setup/build-tools/doupload.sh $targetfile $targetcur
  cd ..
  cd $basedir
  done
cd $basedir


