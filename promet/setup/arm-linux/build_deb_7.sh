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
Subprogram=ocr
echo "copyright and changelog files..."
mkdir -p $BuildDir/usr/share/doc/$Program-$Subprogram
cp ../i386-linux/debian/copyright $BuildDir/usr/share/doc/$Program-$Subprogram/copyright
echo "creating control file..."
mkdir -p $BuildDir/DEBIAN
cat ../i386-linux/debian/control_ocr | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/DEBSIZE/$DebSize/g" \
  > $BuildDir/DEBIAN/control
echo "building package..."
sudo -S dpkg-deb --build $BuildDir
cp $TmpDir/software_build.deb ../output/${Program}-${Subprogram}_${Version}_${Arch}-$Widgetset.deb
echo "cleaning up..."
sudo -S rm -r $BuildDir
