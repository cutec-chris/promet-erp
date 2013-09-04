#!/bin/bash
appname=Promet-ERP
Program=promet-erp
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`
Date=20$Year$Month$Day
TmpDir=/tmp

Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
export CPU_TARGET=$(fpc -iTP)
export DC_APP_DIR=$1/prometerp.app
export DC_INSTALL_DIR=$DC_APP_DIR/Contents/MacOS

mkdir -p $DC_INSTALL_DIR
mkdir -p $DC_INSTALL_DIR/tools
mkdir -p $DC_APP_DIR/Contents/Resources

appfolder=$DC_APP_DIR
macosfolder=$appfolder/Contents/MacOS
plistfile=$appfolder/Contents/Info.plist
appfile=prometerp
 
PkgInfoContents="APPLMAG#"
 
echo "Creating $appfolder..."
mkdir $appfolder
mkdir $appfolder/Contents
mkdir $appfolder/Contents/MacOS
mkdir $appfolder/Contents/Resources

cp ../../output/$CPU_TARGET-darwin/$appfile $macosfolder/$appname
cp ../../output/$CPU_TARGET-darwin/tools/messagemanager $macosfolder/tools/messagemanager
cp ../../output/$CPU_TARGET-darwin/wizardmandant $macosfolder/wizardmandant

# Copy the resource files to the correct place
#  cp *.bmp $appfolder/Contents/Resources
#  cp icon3.ico $appfolder/Contents/Resources
#  cp icon3.png $appfolder/Contents/Resources 
cp ../../resources/world.icns $appfolder/Contents/Resources
#  cp docs/*.* $appfolder/Contents/Resources
#
# Create PkgInfo file.
echo $PkgInfoContents >$appfolder/Contents/PkgInfo
#
# Create information property list file (Info.plist).
echo '<?xml version="1.0" encoding="UTF-8"?>' >$plistfile
echo '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' >>$plistfile
echo '<plist version="1.0">' >>$plistfile
echo '<dict>' >>$plistfile
echo '  <key>CFBundleDevelopmentRegion</key>' >>$plistfile
echo '  <string>English</string>' >>$plistfile
echo '  <key>CFBundleExecutable</key>' >>$plistfile
echo '  <string>'$appname'</string>' >>$plistfile
echo '  <key>CFBundleIconFile</key>' >>$plistfile
echo '  <string>world.icns</string>' >>$plistfile
echo '  <key>CFBundleIdentifier</key>' >>$plistfile
echo '  <string>org.cutec.$appname</string>' >>$plistfile
echo '  <key>CFBundleInfoDictionaryVersion</key>' >>$plistfile
echo '  <string>'$Version'</string>' >>$plistfile
echo '  <key>CFBundlePackageType</key>' >>$plistfile
echo '  <string>APPL</string>' >>$plistfile
echo '  <key>CFBundleSignature</key>' >>$plistfile
echo '  <string>PRM#</string>' >>$plistfile
echo '  <key>CFBundleVersion</key>' >>$plistfile
echo '  <string>1.0</string>' >>$plistfile
echo '</dict>' >>$plistfile
echo '</plist>' >>$plistfile
