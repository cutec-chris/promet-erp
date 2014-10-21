#!/bin/bash
Program=promet-erp
Widgetset=$2
if [ "x$Widgetset" = "x" ]; then
  Widgetset=nogui
fi
Archfpc=$1
if [ "x$Archfpc" = "x" ]; then
  Arch=`dpkg --print-architecture`
  Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
fi
if [ "x$Arch" = "x" ]; then
  if [ "x$Archfpc" = "xx86_64" ]; then
    Arch=amd64
  fi
  if [ "x$Arch" = "x" ]; then
    Arch=$Archfpc
  fi
fi
sudo -S echo "Arch is $Arch"
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

if [ ! -f ../../output/$Archfpc-linux/tools/processmanager ];
then
  echo "ERROR: processmanager fehlt $Archfpc"
  exit
fi

sh build_deb_3.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir $TmpDir #tools
sh build_deb_5.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir $TmpDir #web
sh build_deb_7.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir $TmpDir #ocr
sudo -S rm /tmp/*.deb
sh upload_lin.sh $Arch $Archfpc
sh change_wiki_linux.sh