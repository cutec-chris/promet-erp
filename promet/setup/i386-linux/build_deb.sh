#!/bin/bash
Program=$2
Widgetset=$1
Version=$3
Arch=$4
Archfpc=$5
Date=$6
BuildDir=$7
TmpDir=$8
sudo -S rm -rf $BuildDir
echo "copyright and changelog files..."
mkdir -p $BuildDir/usr/share/doc/$Program
cp debian/changelog.Debian $BuildDir/usr/share/doc/$Program/
cp ../../source/base/changes.txt $BuildDir/usr/share/doc/$Program/changelog
cp debian/copyright $BuildDir/usr/share/doc/$Program/
gzip --best $BuildDir/usr/share/doc/$Program/changelog
gzip --best $BuildDir/usr/share/doc/$Program/changelog.Debian
chmod 644 $BuildDir/usr/share/doc/$Program/*
echo "creating installation..."
mkdir -p $BuildDir/usr/share/pixmaps/
mkdir -p $BuildDir/usr/share/applications
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/usr/lib/$Program
mkdir -p $BuildDir/usr/lib/$Program/languages
mkdir -p $BuildDir/usr/lib/$Program/importdata
install -m 644 ../../resources/world_icon64.png $BuildDir/usr/share/pixmaps/$Program.png
install -m 644 general/$Program.desktop $BuildDir/usr/share/applications/$Program.desktop
install -m 644 general/wizardmandant.desktop $BuildDir/usr/share/applications/prometerp-wizardmandant.desktop
echo "copy to builddir..."
cp ../help/help.db $BuildDir/usr/lib/$Program/
./copy_to_builddir.sh $Archfpc $BuildDir/usr/lib/$Program
install general/$Program.starter $BuildDir/usr/lib/$Program/
install general/helpviewer.starter $BuildDir/usr/lib/$Program/
install general/wizardmandant.starter $BuildDir/usr/lib/$Program/
ln -s /usr/lib/$Program/helpviewer.starter $BuildDir/usr/bin/promet-erp-help
chmod 666 $BuildDir/usr/bin/promet-erp-help
ln -s /usr/lib/$Program/$Program.starter $BuildDir/usr/bin/$Program
chmod 666 $BuildDir/usr/bin/$Program
ln -s /usr/lib/$Program/wizardmandant.starter $BuildDir/usr/bin/promet-erp-wizardmandant
chmod 666 $BuildDir/usr/bin/promet-erp-wizardmandant
cp -r ../../importdata/* $BuildDir/usr/lib/$Program/importdata
sudo -S chmod -R 644 $BuildDir/usr/lib/$Program/importdata/
cp ../../languages/*.po $BuildDir/usr/lib/$Program/languages
cp ../../languages/*.txt $BuildDir/usr/lib/$Program/languages
cp ../warnings.txt $BuildDir/usr/lib/$Program
cp ../errors.txt $BuildDir/usr/lib/$Program
cp add-systray-icon.sh $BuildDir/usr/lib/$Program
sudo -S chmod -R 644 $BuildDir/usr/lib/$Program/languages/
./copy_to_builddir_sync.sh $Archfpc $BuildDir/usr/lib/$Program
ln -s /usr/lib/$Program/tools/sync_db $BuildDir/usr/bin/promet-erp-sync_db
chmod 666 $BuildDir/usr/bin/promet-erp-sync_db
DebSize=$(du -s $BuildDir | cut -f1)
echo "fixing permissions ..."
sudo -S find $BuildDir -type d -print0 | xargs -0 sudo -S chmod 755  # this is needed, don't ask me why
sudo -S find $BuildDir -type f -print0 | xargs -0 sudo -S chmod a+r  # this is needed, don't ask me why
sudo -S chown -hR root:root $BuildDir/usr
echo "creating control file..."
mkdir -p $BuildDir/DEBIAN
cat debian/control | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/DEBSIZE/$DebSize/g" \
  > $BuildDir/DEBIAN/control
chmod 755 $BuildDir/DEBIAN
if [ ! -f ../../output/$Archfpc-linux/cdmenue ];
then
  echo "cdmenue build dont exists (../../output/$Archfpc-linux/cdmenue)..."
  exit
fi
echo "building package..."
sudo -S dpkg-deb --build $BuildDir
cp $TmpDir/software_build.deb ../output/${Program}_${Version}_${Arch}-$Widgetset.deb
./build_tar.sh $Widgetset $Program $Version $Arch $Archfpc $Date $BuildDir
mv $BuildDir/${Program}*.tar.gz ../output
echo "cleaning up..."
sudo -S rm -r $BuildDir
