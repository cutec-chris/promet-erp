#!/bin/bash
basedir=$(pwd)
cd promet/setup/portableapps
echo "Building portableaps stuff..."
TmpDir=$TMP
BuildDir=$TmpDir/software_build
echo "cleaning up..."
rm -rf $BuildDir
echo "copy to builddir..."
mkdir -p $BuildDir/App/AppInfo
cp -r ./Promet-ERP/* $BuildDir
mkdir -p $BuildDir/App/promet/tools
mkdir -p $BuildDir/App/promet/plugins
# Create Install Dir
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/prometerp_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/help-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/importdata-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/messagemanager_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/plugins_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/visualtools_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/sqliteclient_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/win32tools_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/sync_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
rm $BuildDir/App/promet/helpviewer.exe
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/mailreceiver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
echo "building package..."
rm $TmpDir/*.paf.exe
cat Appinfo_devel.ini | \
  $SED -b -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > $BuildDir/App/AppInfo/appinfo.ini
rm $BuildDir/App/AppInfo/Launcher/Splash.jpg
/c/Windows/system32/cmd.exe "/C c:\PortableApps.comInstaller\PortableApps.comInstaller.exe %TEMP%\software_build
"
cp $TmpDir/*$BUILD_VERSION.paf.exe ../output/$BUILD_VERSION
targetfile=PrometERPPortable_$BUILD_VERSION.paf.exe
targetcur=PrometERPPortable_current.paf.exe
if [ "$1" = "upload" ]; then
  . ../build-tools/doupload.sh $targetfile $targetcur
fi
rm $TmpDir/*.paf.exe
cat Appinfo.ini | \
  $SED -b -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > $BuildDir/App/AppInfo/appinfo.ini
/c/Windows/system32/cmd.exe "/C c:\PortableApps.comInstaller\PortableApps.comInstaller.exe %TEMP%\software_build
"
cp $TmpDir/*.paf.exe ../output
targetfile=PrometERPPortable_$BUILD_VERSION_Development_Test_1.paf.exe
targetcur=PrometERPPortable_current_Development_Test_1.paf.exe
if [ "$1" = "upload" ]; then
  . ../build-tools/doupload.sh $targetfile $targetcur
fi
cd $BuildDir
rm        $basedir/promet/setup/output/promet-erp-$Version.i386-win32-portable.zip
zip -9 -r $basedir/promet/setup/output/promet-erp-$Version.i386-win32-portable.zip .
cd $basedir
cd promet/setup/portableapps
targetfile=promet-erp-$BUILD_VERSION.i386-win32-portable.zip
targetcur=promet-erp-current-win32-portable.zip
if [ "$1" = "upload" ]; then
  . ../build-tools/doupload.sh $targetfile $targetcur
fi

#Build Firebird Version
#cp -r ../i386-win32/firebird-embedded/* $BuildDir/Promet-ERP/App/promet
#cat Appinfo_firebird.ini | \
#  sed -b -e "s/VERSION/$Version/g" \
#      -e "s/ARCH/$Arch/g" \
#      -e "s/ARCHFPC/$Archfpc/g" \
#      -e "s/CREATEDDATE/$Date/g" \
#  > $BuildDir/App/AppInfo/appinfo.ini
#/c/Windows/system32/cmd.exe "/C c:\PortableApps.comInstaller\PortableApps.comInstaller.exe %TEMP%\software_build
#"
#cp $TmpDir/*.paf.exe ../output
#cd $BuildDir
#targetfile=PrometERPPortable-firebird_$BUILD_VERSION.paf.exe
#targetcur=PrometERPPortable-firebird_current.paf.exe
#if [ "$1" = "upload" ]; then
#  . ../build-tools/doupload.sh $targetfile $targetcur
#fi
#rm        $basedir/promet/setup/output/promet-erp-firebird-$Version.i386-win32-portable.zip
#zip -9 -r $basedir/promet/setup/output/promet-erp-firebird-$Version.i386-win32-portable.zip .
#targetfile=promet-erp-$BUILD_VERSION-win32-portable.zip
#targetcur=promet-erp-firebird-current-win32-portable.zip
#if [ "$1" = "upload" ]; then
#  . ../build-tools/doupload.sh $targetfile $targetcur
#fi

if [ "$1" = "upload" ]; then
  . change_wiki.sh
fi

cd $basedir


