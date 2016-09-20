#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-linux
. ../../setup/build-tools/setup_enviroment.sh
echo "Uploading linux packages..."
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

Program="promet-erp"
. ../../setup/build-tools/doupload.sh ${Program}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}_current_${Arch}-$Widgetset.deb

SubProgram="statistics"
. ../../setup/build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb

SubProgram="timeregistering"
. ../../setup/build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb

SubProgram="services"
. ../../setup/build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb

SubProgram="ocr"
. ../../setup/build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb

SubProgram="aqbanking"
. ../../setup/build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb
cd $basedir


