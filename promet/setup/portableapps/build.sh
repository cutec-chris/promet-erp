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
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/$BUILD_VERSION/prometerp_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/$BUILD_VERSION/help-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/$BUILD_VERSION/importdata-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/$BUILD_VERSION/messagemanager_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/$BUILD_VERSION/plugins_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/$BUILD_VERSION/visualtools_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
rm $BuildDir/App/promet/helpviewer.exe
unzip -u -d $BuildDir/App/promet $basedir/promet/setup/output/$BUILD_VERSION/mailreceiver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
echo "building package..."
cat Appinfo_devel.ini | \
  sed -b -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > $BuildDir/App/AppInfo/appinfo.ini
rm $BuildDir/App/AppInfo/Launcher/Splash.jpg
/c/Windows/system32/cmd.exe "/C c:\PortableApps.comInstaller\PortableApps.comInstaller.exe %TEMP%\software_build
"
cat Appinfo.ini | \
  sed -b -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > $BuildDir/App/AppInfo/appinfo.ini
/c/Windows/system32/cmd.exe "/C c:\PortableApps.comInstaller\PortableApps.comInstaller.exe %TEMP%\software_build
"
cp $TmpDir/*.paf.exe ../output
cd $BuildDir
rm        $basedir/promet/setup/output/promet-erp-$Version.i386-win32-portable.zip
zip -9 -r $basedir/promet/setup/output/promet-erp-$Version.i386-win32-portable.zip .

#Build Firebird Version
cp -r ../i386-win32/firebird-embedded/* $BuildDir/Promet-ERP/App/promet
cat Appinfo_firebird.ini | \
  sed -b -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > $BuildDir/App/AppInfo/appinfo.ini
/c/Windows/system32/cmd.exe "/C c:\PortableApps.comInstaller\PortableApps.comInstaller.exe %TEMP%\software_build
"
cp $TmpDir/*.paf.exe ../output
cd $BuildDir
rm        $basedir/promet/setup/output/promet-erp-firebird-$Version.i386-win32-portable.zip
zip -9 -r $basedir/promet/setup/output/promet-erp-firebird-$Version.i386-win32-portable.zip .

cd $basedir


