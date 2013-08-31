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
Subprogram=statistics
echo "copyright and changelog files..."
mkdir -p $BuildDir/usr/share/doc/$Program-$Subprogram
cp debian/changelog.Debian $BuildDir/usr/share/doc/$Program-$Subprogram/
cp ../../source/base/changes.txt $BuildDir/usr/share/doc/$Program-$Subprogram/changelog
cp debian/copyright $BuildDir/usr/share/doc/$Program-$Subprogram/copyright
gzip --best $BuildDir/usr/share/doc/$Program-$Subprogram/changelog
gzip --best $BuildDir/usr/share/doc/$Program-$Subprogram/changelog.Debian
chmod 644 $BuildDir/usr/share/doc/$Program-$Subprogram/*
echo "creating installation..."
mkdir -p $BuildDir/usr/share/pixmaps/
mkdir -p $BuildDir/usr/share/applications
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/usr/lib/$Program
install -m 644 ../../resources/world_icon_statistics.png $BuildDir/usr/share/pixmaps/prometerp-statistics.png
install -m 644 general/statistics.desktop $BuildDir/usr/share/applications/prometerp-statistics.desktop
echo "copy to builddir..."
./copy_to_builddir_statistics.sh $Archfpc $BuildDir/usr/lib/$Program
cp general/statistics.starter $BuildDir/usr/lib/$Program/
ln -s /usr/lib/$Program/statistics.starter $BuildDir/usr/bin/promet-erp-statistics
chmod 666 $BuildDir/usr/bin/promet-erp-statistics
DebSize=$(du -s $BuildDir | cut -f1)
echo "fixing permissions ..."
find $BuildDir -type d -print0 | xargs -0 sudo -S chmod 755  # this is needed, don't ask me why
find $BuildDir -type f -print0 | xargs -0 sudo -S chmod a+r  # this is needed, don't ask me why
sudo -S chown -hR root:root $BuildDir/usr
echo "creating control file..."
mkdir -p $BuildDir/DEBIAN
cat debian/control_statistics | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/DEBSIZE/$DebSize/g" \
  > $BuildDir/DEBIAN/control
chmod 755 $BuildDir/DEBIAN
echo "building package..."
sudo -S dpkg-deb --build $BuildDir
cp $TmpDir/software_build.deb ../output/${Program}-statistics_${Version}_${Arch}-$Widgetset.deb
./build_tar.sh $Widgetset $Program-statistics $Version $Arch $Archfpc $Date $BuildDir
mv $BuildDir/${Program}*.tar.gz ../output
echo "cleaning up..."
sudo -S rm -r $BuildDir
