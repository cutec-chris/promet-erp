#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-linux
. ../../setup/build-tools/setup_enviroment.sh
echo "Uploading linux packages..."
# Packages
cd $basedir
cd promet/setup/output
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`
Date=20$Year$Month$Day
TmpDir=/tmp
BuildDir=$TmpDir/software_build
TARGET_CPU=$1
TARGET_OS=$2
Arch=$TARGET_CPU
if [ "x$Arch" = "xx86_64" ]; then
  Arch=amd64
fi

Program="promet-erp"
. ../build-tools/doupload.sh ${Program}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}_current_${Arch}-$Widgetset.deb

SubProgram="statistics"
. ../build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb

SubProgram="timeregistering"
. ../build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb

SubProgram="services"
. ../build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb

SubProgram="ocr"
. ../build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb

SubProgram="aqbanking"
. ../build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb
cd $basedir


