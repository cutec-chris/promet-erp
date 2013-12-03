#!/bin/bash
Version=$(sed 's/\r//g' ../source/base/version.inc).$(sed 's/\r//g' ../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
sudo -S rm output/*
mkdir executables/$Version
mkdir executables/$Version/x86_64
mkdir executables/$Version/i386
lazbuild ../source/testcases/consoletest.lpi
../output/x86_64-linux/consoletest --mandant=Test
if [ "$?" -ne "0" ]; then
  echo "Testcases failed exitting"
  exit 1
fi
sudo -S ./clean_all.sh
virsh start Autobuild_lin3
sh build_win_wine_i386.sh &
State=$(virsh domstate Autobuild_lin3)
while [ "$State" = laufend ] ; do
  sleep 5
  State=$(virsh domstate Autobuild_lin3)
done
while [ "$State" = running ] ; do
  sleep 5
  State=$(virsh domstate Autobuild_lin3)
done
echo $State
sh upload_lin.sh i386 i386 &
BASEDIR=$(dirname $0)
C_DIR=`pwd`
if test "`dirname $0`" = "."
then
    FULL_NAME=$C_DIR/$BASEDIR
else
    FULL_NAME=$BASEDIR
fi
cd i386-linux
./build_all.sh
cd ..
./upload_lin.sh amd64 x86_64
./change_wiki.sh
ssh chris@minimac 'sh promet/promet/setup/build_all.mac' &
#cd $FULL_NAME/zip-files
#./build_stick.sh
#cd ..
