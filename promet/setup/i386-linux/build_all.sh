#!/bin/bash
Program=promet-erp
Widgetset=$1
if [ "x$Widgetset" = "x" ]; then
  Widgetset=gtk2
fi
Arch=`dpkg --print-architecture`
sudo -S echo "Arch is $Arch"
Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
echo "Archfpc is $Archfpc"
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`
Date=20$Year$Month$Day
TmpDir=/tmp
BuildDir=$TmpDir/software_build
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
echo "Build directory is $BuildDir"
if [ x$BuildDir = x/ ]; then
  echo "ERROR: invalid build directory"
  exit
fi

export PATH=$PATH:/home/chris/lazarus:/media/2DF0E8714D527894/lazarus
echo build_all_executables.sh $Widgetset $Archfpc
sh build_all_executables.sh $Widgetset $Archfpc

if [ ! -f ../../output/$Archfpc-linux/prometerp ];
then
  echo "ERROR: prometerp fehlt"
  exit
fi
if [ ! -f ../../output/$Archfpc-linux/tools/messagemanager ];
then
  echo "ERROR: messagemanager fehlt"
  exit
fi
if [ ! -f ../../output/$Archfpc-linux/pstarter ];
then
  echo "ERROR: pstarter fehlt"
  exit
fi
if [ ! -f ../../output/$Archfpc-linux/tools/processmanager ];
then
  echo "ERROR: processmanager fehlt"
  exit
fi

echo build_deb.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir $TmpDir
sh build_deb.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir $TmpDir
if [ ! -f ../../output/$Archfpc-linux/cdmenue ];
then
    exit
fi
sh build_deb_2.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir $TmpDir #statistics
sh build_deb_3.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir $TmpDir #tools
sh build_deb_4.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir $TmpDir #timeregistering
sh build_deb_5.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir $TmpDir #web
sh build_rpm.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir $TmpDir
cp /tmp/*.rpm ../output
sudo -S rm /tmp/*.rpm
sudo -S rm /tmp/*.deb