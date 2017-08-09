#!/bin/bash

add_std_files()
{
  echo "copyright and changelog files..."
  mkdir -p $BuildDir/usr/share/doc/$Program$Subprogram
  cp debian/changelog.Debian $BuildDir/usr/share/doc/$Program$Subprogram/
  cp ../../source/base/changes.txt $BuildDir/usr/share/doc/$Program$Subprogram/changelog
  cp debian/copyright $BuildDir/usr/share/doc/$Program$Subprogram/copyright
  gzip --best $BuildDir/usr/share/doc/$Program$Subprogram/changelog
  gzip --best $BuildDir/usr/share/doc/$Program$Subprogram/changelog.Debian
  sudo -S chmod 644 $BuildDir/usr/share/doc/$Program$Subprogram/*
}

build_deb()
{
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
echo "Building linux packages..."
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
sudo -S rm -rf /tmp/*
fakeroot rm -rf $BuildDir
Program="promet-erp"
mkdir -p $BuildDir/usr/bin/

SubProgram="services"
fakeroot rm -rf $BuildDir
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/usr/lib/$Program
add_std_files;
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/tools_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/davserver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/feedreceiver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/fhemreceiver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/mqttreceiver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/imapserver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/sync_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/webserver_$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
unzip -u -d $BuildDir/usr/lib/$Program $basedir/promet/setup/output/help-$BUILD_VERSION.zip
mkdir -p $BuildDir/usr/share/pixmaps/
mkdir -p $BuildDir/usr/share/applications
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/usr/lib/$Program
mkdir -p $BuildDir/etc/init.d/

git clone https://github.com/cutec-chris/promet-apps $BuildDir/usr/lib/$Program/web2
cd $BuildDir/usr/lib/$Program/web2
git submodule init
git submodule update

rm $BuildDir/usr/lib/$Program/web2/*.md
rm $BuildDir/usr/lib/$Program/web2/LICENSE
rm $BuildDir/usr/lib/$Program/web2/.gitmodules

ln -s /usr/lib/$Program/tools/imapserver $BuildDir/usr/bin/promet-erp-imap
ln -s /usr/lib/$Program/tools/davserver $BuildDir/usr/bin/promet-erp-dav
ln -s /usr/lib/$Program/tools/webserver $BuildDir/usr/bin/promet-erp-appbase
ln -s /usr/lib/$Program/tools/mta $BuildDir/usr/bin/promet-erp-mta
ln -s /usr/lib/$Program/tools/nntpserver $BuildDir/usr/bin/promet-erp-nntp
ln -s /usr/lib/$Program/tools/syslog $BuildDir/usr/bin/promet-erp-syslog
chmod 666 $BuildDir/usr/bin/promet-erp-imap
chmod 666 $BuildDir/usr/bin/promet-erp-dav
chmod 666 $BuildDir/usr/bin/promet-erp-appbase
chmod 666 $BuildDir/usr/bin/promet-erp-mta
chmod 666 $BuildDir/usr/bin/promet-erp-nntp

cp ../i386-linux/debian/promet-process.sh $BuildDir/etc/init.d/promet-process
chmod 666 $BuildDir/etc/init.d/promet-process
chmod +x $BuildDir/etc/init.d/promet-process

install -m 644 ../../resources/world_icon_statistics.png $BuildDir/usr/share/pixmaps/prometerp-statistics.png
install -m 644 general/statistics.desktop $BuildDir/usr/share/applications/prometerp-statistics.desktop
chmod 666 $BuildDir/usr/bin/promet-erp-statistics
build_deb;
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb
fi

SubProgram="ocr"
fakeroot rm -rf $BuildDir
add_std_files;
build_deb;
if [ "$1" = "upload" ]; then
  . ../../setup/build-tools/doupload.sh ${Program}-${SubProgram}_${BUILD_VERSION}_${Arch}-$TARGET_WIDGETSET.deb ${Program}-${SubProgram}_current_${Arch}-$Widgetset.deb
fi

cd $basedir


