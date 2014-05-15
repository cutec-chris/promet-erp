#!/bin/bash
Program=promet-erp
Widgetset=$2
if [ "x$Widgetset" = "x" ]; then
  Widgetset=gtk2
fi
Archfpc=i386
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
sudo -S rm -rf $BuildDir
echo "copy to builddir..."
cp -r Promet-ERP $BuildDir/Promet-ERP
mkdir -p $BuildDir/Promet-ERP/App/promet-erp/tools
cp -r ../../importdata $BuildDir/Promet-ERP/App/promet-erp
cp ../help/help.db $BuildDir/Promet-ERP/App/promet-erp
cp ../warnings.txt $BuildDir/Promet-ERP/App/promet-erp
cp ../errors.txt $BuildDir/Promet-ERP/App/promet-erp
cp ../executables/$Version/$Archfpc/prometerp.exe $BuildDir/Promet-ERP/App/promet-erp
cp ../executables/$Version/$Archfpc/pstarter.exe $BuildDir/Promet-ERP/App/promet-erp
cp ../executables/$Version/$Archfpc/messagemanager.exe $BuildDir/Promet-ERP/App/promet-erp/tools
cp ../executables/$Version/$Archfpc/wizardmandant.exe $BuildDir/Promet-ERP/App/promet-erp
cp ../executables/$Version/$Archfpc/*sender.exe $BuildDir/Promet-ERP/App/promet-erp/tools
cp ../executables/$Version/$Archfpc/*receiver.exe $BuildDir/Promet-ERP/App/promet-erp/tools
cp ../executables/$Version/$Archfpc/helpviewer.exe $BuildDir/Promet-ERP/App/promet-erp
#cp ../executables/$Version/$Archfpc/sync_*.exe $BuildDir/Promet-ERP/App/promet-erp/tools
cp ../i386-win32/sqlite3.dll $BuildDir/Promet-ERP/App/promet-erp
cp ../i386-win32/sqlite3.dll $BuildDir/Promet-ERP/App/promet-erp/tools
echo "building package..."
echo "cleaning up..."
#sudo -S rm -r $BuildDir
