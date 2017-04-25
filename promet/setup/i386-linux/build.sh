#!/bin/bash

add_std_files()
{
  echo "\e[33mAdding Std Files for ${Program}-${SubProgram}"
  echo "copyright and changelog files..."
  mkdir -p $BuildDir/usr/share/doc/$Program$Subprogram
  cp debian/changelog.Debian $BuildDir/usr/share/doc/$Program$Subprogram/
  cp ../../source/base/changes.txt $BuildDir/usr/share/doc/$Program$Subprogram/changelog
  cp debian/copyright $BuildDir/usr/share/doc/$Program$Subprogram/copyright
  gzip --best $BuildDir/usr/share/doc/$Program$Subprogram/changelog
  gzip --best $BuildDir/usr/share/doc/$Program$Subprogram/changelog.Debian
  fakeroot chmod 644 $BuildDir/usr/share/doc/$Program$Subprogram/*
}

build_deb()
{
  echo "\e[33mbuilding deb for ${Program}-${SubProgram}"
  echo "fixing permissions ..."
  fakeroot find $BuildDir -type d -print0 | xargs -0 sudo -S chmod 755  # this is needed, don't ask me why
  fakeroot find $BuildDir -type f -print0 | xargs -0 sudo -S chmod a+r  # this is needed, don't ask me why
  fakeroot chown -hR root:root $BuildDir/usr
  DebSize=$(sudo du -s $BuildDir | cut -f1)
  echo "creating control file..."
  mkdir -p $BuildDir/DEBIAN
  cat debian/control_${SubProgram} | \
    sed -e "s/VERSION/$BUILD_VERSION/g" \
        -e "s/ARCH/$Arch/g" \
        -e "s/DEBSIZE/$DebSize/g" \
    > $BuildDir/DEBIAN/control
  chmod 755 $BuildDir/DEBIAN
  echo "building package..."
  fakeroot dpkg-deb --build $BuildDir
  if [ "x${SubProgram}" = "x" ]; then
    cp $TmpDir/software_build.deb ../output/${Program}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb
  elif [ "x${SubProgram}" <> "x" ]; then
    cp $TmpDir/software_build.deb ../output/${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb
  fi
}

basedir=$(pwd)
cd promet/setup/i386-linux
. ../../setup/build-tools/setup_enviroment.sh
echo "\e[33mBuilding linux packages..."
# Packages
cd $basedir
cd promet/setup/i386-linux
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`
Date=20$Year$Month$Day
TmpDir=/tmp
BuildDir=$TmpDir/software_build
Arch=$TARGET_CPU
if [ "x$Arch" = "xx86_64" ]; then
  Arch=amd64
fi


sudo -S rm -rf $BuildDir
fakeroot rm -rf $BuildDir
Program="promet-erp"
SubProgram=""
mkdir -p $BuildDir
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/usr/lib/$Program
add_std_files;
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/prometerp_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/help-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/importdata-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/messagemanager_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/plugins_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/visualtools_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/mailreceiver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
mkdir -p $BuildDir/usr/share/pixmaps/
install -m 644 ../../resources/world_icon64.png $BuildDir/usr/share/pixmaps/$Program.png
mkdir -p $BuildDir/usr/share/applications/
install -m 644 general/$Program.desktop $BuildDir/usr/share/applications/$Program.desktop
install -m 644 general/wizardmandant.desktop $BuildDir/usr/share/applications/prometerp-wizardmandant.desktop
install general/$Program.starter $BuildDir/usr/lib/$Program/
install general/helpviewer.starter $BuildDir/usr/lib/$Program/
install general/wizardmandant.starter $BuildDir/usr/lib/$Program/
ln -s /usr/lib/$Program/helpviewer.starter $BuildDir/usr/bin/promet-erp-help
chmod 777 $BuildDir/usr/bin/promet-erp/help.db
chmod 666 $BuildDir/usr/bin/promet-erp-help
ln -s /usr/lib/$Program/$Program.starter $BuildDir/usr/bin/$Program
chmod 666 $BuildDir/usr/bin/$Program
ln -s /usr/lib/$Program/wizardmandant.starter $BuildDir/usr/bin/promet-erp-wizardmandant
chmod 666 $BuildDir/usr/bin/promet-erp-wizardmandant
mkdir -p $BuildDir/usr/lib/$Program/languages
cp ../../languages/*.po $BuildDir/usr/lib/$Program/languages
cp ../../languages/*.txt $BuildDir/usr/lib/$Program/languages
cp ../warnings.txt $BuildDir/usr/lib/$Program
cp ../errors.txt $BuildDir/usr/lib/$Program
cp add-systray-icon.sh $BuildDir/usr/lib/$Program
fakeroot chmod -R 644 $BuildDir/usr/lib/$Program/languages/
build_deb;
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh ${Program}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}_current_${Arch}-$Widgetset.deb
fi

SubProgram="statistics"
fakeroot rm -rf $BuildDir
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/usr/lib/$Program
add_std_files;
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/statistics_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
mkdir -p $BuildDir/usr/share/pixmaps/
mkdir -p $BuildDir/usr/share/applications
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/usr/lib/$Program
install -m 644 ../../resources/world_icon_statistics.png $BuildDir/usr/share/pixmaps/prometerp-statistics.png
install -m 644 general/statistics.desktop $BuildDir/usr/share/applications/prometerp-statistics.desktop
ln -s /usr/lib/$Program/statistics.starter $BuildDir/usr/bin/promet-erp-statistics
chmod 666 $BuildDir/usr/bin/promet-erp-statistics
build_deb;
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb
fi

SubProgram="timeregistering"
fakeroot rm -rf $BuildDir
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/usr/lib/$Program
add_std_files;
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/timeregistering_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
mkdir -p $BuildDir/usr/share/pixmaps/
mkdir -p $BuildDir/usr/share/applications
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/usr/lib/$Program
install -m 644 ../../resources/world_icon64.png $BuildDir/usr/share/pixmaps/prometerp-timeregistering.png
install -m 644 general/timeregistering.desktop $BuildDir/usr/share/applications/prometerp-timeregistering.desktop
ln -s /usr/lib/$Program/timeregistering.starter $BuildDir/usr/bin/promet-erp-timeregistering
chmod 666 $BuildDir/usr/bin/promet-erp-statistics
build_deb;
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb
fi

SubProgram="aqbanking"
fakeroot rm -rf $BuildDir
add_std_files;
build_deb;
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb
fi
cd $basedir


