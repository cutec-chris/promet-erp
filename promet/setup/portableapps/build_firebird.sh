#!/bin/bash
Program=Promet-ERP
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
sudo -S sh clean_all.sh
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
mkdir -p $BuildDir/Promet-ERP
cp -r ./Promet-ERP_Firebird/* $BuildDir/Promet-ERP
mkdir -p $BuildDir/Promet-ERP/App/promet/tools
mkdir -p $BuildDir/Promet-ERP/App/promet/plugins
cp -r ../i386-win32/tools $BuildDir/Promet-ERP/App/promet
cp -r ../../importdata $BuildDir/Promet-ERP/App/promet/
mv "$BuildDir/Promet-ERP/App/promet/importdata/Warenwirtschaft (alles)/Österreich" "$BuildDir/Promet-ERP/App/promet/importdata/Warenwirtschaft (alles)/Osterreich"
cp ../help.db $BuildDir/Promet-ERP/App/promet
cp ../warnings.txt $BuildDir/Promet-ERP/App/promet
cp ../errors.txt $BuildDir/Promet-ERP/App/promet
cp ../executables/$Version/$Archfpc/prometerp.exe $BuildDir/Promet-ERP/App/promet
cp ../executables/$Version/$Archfpc/pstarter.exe $BuildDir/Promet-ERP/App/promet
cp ../executables/$Version/$Archfpc/messagemanager.exe $BuildDir/Promet-ERP/App/promet/tools
cp ../executables/$Version/$Archfpc/wizardmandant.exe $BuildDir/Promet-ERP/App/promet
cp ../executables/$Version/$Archfpc/*sender.exe $BuildDir/Promet-ERP/App/promet/tools
cp ../executables/$Version/$Archfpc/*receiver.exe $BuildDir/Promet-ERP/App/promet/tools
cp ../executables/$Version/$Archfpc/*.wlx $BuildDir/Promet-ERP/App/promet/plugins
cp ../executables/$Version/$Archfpc/helpviewer.exe $BuildDir/Promet-ERP/App/promet
cp ../executables/$Version/$Archfpc/sync_db.exe $BuildDir/Promet-ERP/App/promet/tools
cp ../i386-win32/sqlite3.dll $BuildDir/Promet-ERP/App/promet
cp ../i386-win32/sqlite3.dll $BuildDir/Promet-ERP/App/promet/tools
mkdir -p $BuildDir/Promet-ERP/App/promet/languages
cp -r ../../languages/*.en.po $BuildDir/Promet-ERP/App/promet/languages
cp -r ../../languages/*.de.po $BuildDir/Promet-ERP/App/promet/languages
cp -r ../../languages/languages.txt $BuildDir/Promet-ERP/App/promet/languages
cp -r ../i386-win32/firebird-embedded/* $BuildDir/Promet-ERP/App/promet

echo "compressing..."
FULL_NAME=$(cd `dirname $0` && pwd)
WIN_DIR=$(echo $FULL_NAME | sed 's/\//\\/g')
WIN_DIR='Z:\'$WIN_DIR
#WINEPREFIX=$FULL_NAME/../../../lazarus_wine/ wineconsole "$WIN_DIR\compress.bat" 'Z:'$(echo $BuildDir | sed 's/\//\\/g') $WIN_DIR
echo "building package..."
cat Appinfo_devel.ini | \
  $SED -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > $BuildDir/Promet-ERP/App/AppInfo/Appinfo.ini
#WINEPREFIX=$FULL_NAME/../../../lazarus_wine/ wine "c:\PortableApps.comInstaller\PortableApps.comInstaller.exe" 'Z:'$(echo $BuildDir | sed 's/\//\\/g')'\Promet-ERP'
rm $BuildDir/Promet-ERP/App/AppInfo/Launcher/Splash.jpg
cat Appinfo.ini | \
  $SED -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > $BuildDir/Promet-ERP/App/AppInfo/Appinfo.ini
WINEPREFIX=$FULL_NAME/../../../lazarus_wine/ wine "c:\PortableApps.comInstaller\PortableApps.comInstaller.exe" 'Z:'$(echo $BuildDir | sed 's/\//\\/g')'\Promet-ERP'
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
cp $BuildDir/*.paf.exe ../output
cd $BuildDir
rm $FULL_NAME/../output/promet-erp-$(echo $Version).i386-win32-portable-firebird.zip
zip -9 -r $FULL_NAME/../output/promet-erp-$(echo $Version).i386-win32-portable-firebird.zip Promet-ERP
echo "cleaning up..."
#sudo -S rm -r $BuildDir
