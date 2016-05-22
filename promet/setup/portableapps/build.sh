#!/bin/bash
basedir=$(pwd)
cd promet/setup/portableapps
echo "Building portableaps stuff..."
TmpDir=%TEMP%
BuildDir=$TmpDir/software_build
rm -rf $BuildDir
echo "copy to builddir..."
mkdir -p $BuildDir
# Create Install Dir
unzip -u -d $BuildDir $basedir/promet/setup/output/$BUILD_VERSION/prometerp_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir $basedir/promet/setup/output/$BUILD_VERSION/help-$BUILD_VERSION.zip
unzip -u -d $BuildDir $basedir/promet/setup/output/$BUILD_VERSION/importdata-$BUILD_VERSION.zip
unzip -u -d $BuildDir $basedir/promet/setup/output/$BUILD_VERSION/messagemanager_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir $basedir/promet/setup/output/$BUILD_VERSION/plugins_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir $basedir/promet/setup/output/$BUILD_VERSION/visualtools_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir $basedir/promet/setup/output/$BUILD_VERSION/mailreceiver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
echo "building package..."
cat Appinfo_devel.ini | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > $BuildDir/App/AppInfo/Appinfo.ini
rm $BuildDir/App/AppInfo/Launcher/Splash.jpg
cat Appinfo.ini | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > $BuildDir/App/AppInfo/Appinfo.ini
c:\PortableApps.comInstaller\PortableApps.comInstaller.exe '$BuildDir'
cp $BuildDir/*.paf.exe ../output
cd $BuildDir
rm $basedir/../output/promet-erp-$(echo $Version).i386-win32-portable.zip
zip -9 -r $basedir/../output/promet-erp-$(echo $Version).i386-win32-portable.zip Promet-ERP
echo "cleaning up..."

cd $basedir


