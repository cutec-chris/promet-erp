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
Subprogram=webservices
echo "copyright and changelog files..."
mkdir -p $BuildDir/usr/share/doc/$Program-$Subprogram
cp ../i386-linux/debian/changelog.Debian $BuildDir/usr/share/doc/$Program-$Subprogram/
cp ../../source/base/changes.txt $BuildDir/usr/share/doc/$Program-$Subprogram/changelog
cp ../i386-linux/debian/copyright $BuildDir/usr/share/doc/$Program-$Subprogram/copyright
gzip --best $BuildDir/usr/share/doc/$Program-$Subprogram/changelog
gzip --best $BuildDir/usr/share/doc/$Program-$Subprogram/changelog.Debian
chmod 644 $BuildDir/usr/share/doc/$Program-$Subprogram/*
echo "creating installation..."
mkdir -p $BuildDir/usr/share/pixmaps/
mkdir -p $BuildDir/usr/share/applications
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/etc/
mkdir -p $BuildDir/etc/init.d/
mkdir -p $BuildDir/usr/lib/$Program
echo "copy to builddir..."
./copy_to_builddir_web.sh $Archfpc $BuildDir/usr/lib/$Program
cp ../i386-linux/debian/promet-process.sh $BuildDir/etc/init.d/promet-process
chmod 666 $BuildDir/etc/init.d/promet-process
chmod +x $BuildDir/etc/init.d/promet-process
ln -s /usr/lib/$Program/tools/imapserver $BuildDir/usr/bin/promet-erp-imap
ln -s /usr/lib/$Program/tools/webdavserver $BuildDir/usr/bin/promet-erp-webdav
ln -s /usr/lib/$Program/tools/local_appbase $BuildDir/usr/bin/promet-erp-appbase
ln -s /usr/lib/$Program/tools/mta $BuildDir/usr/bin/promet-erp-mta
ln -s /usr/lib/$Program/tools/nntpserver $BuildDir/usr/bin/promet-erp-nntp
ln -s /usr/lib/$Program/tools/syslog $BuildDir/usr/bin/promet-erp-syslog
chmod 666 $BuildDir/usr/bin/promet-erp-imap
chmod 666 $BuildDir/usr/bin/promet-erp-webdav
chmod 666 $BuildDir/usr/bin/promet-erp-appbase
chmod 666 $BuildDir/usr/bin/promet-erp-mta
chmod 666 $BuildDir/usr/bin/promet-erp-nntp
ln -s /usr/lib/$Program/cmdwizardmandant $BuildDir/usr/bin/promet-erp-cmdwizardmandant
chmod 666 $BuildDir/usr/bin/promet-erp-cmdwizardmandant
DebSize=$(du -s $BuildDir | cut -f1)
echo "fixing permissions ..."
find $BuildDir -type d -print0 | xargs -0 sudo -S chmod 755  # this is needed, don't ask me why
find $BuildDir -type f -print0 | xargs -0 sudo -S chmod a+r  # this is needed, don't ask me why
sudo -S chown -hR root:root $BuildDir/usr
echo "build tar.gz..."
tar -cvf - $BuildDir | gzip > ../output/${Program}-${Subprogram}_$Version_$Arch-$Widgetset.tar.gz
echo "creating control file..."
mkdir -p $BuildDir/DEBIAN
cat ../i386-linux/debian/control_services | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/DEBSIZE/$DebSize/g" \
  > $BuildDir/DEBIAN/control
cp ../i386-linux/debian/prerm $BuildDir/DEBIAN/prerm
chmod 755 $BuildDir/DEBIAN
cp ../i386-linux/debian/postinst $BuildDir/DEBIAN/postinst
chmod 755 $BuildDir/DEBIAN/prerm
chmod 755 $BuildDir/DEBIAN/postinst
echo "building package..."
sudo -S dpkg-deb --build $BuildDir
cp $TmpDir/software_build.deb ../output/${Program}-${Subprogram}_${Version}_${Arch}-$Widgetset.deb
./build_tar.sh $Widgetset $Program-tools $Version $Arch $Archfpc $Date $BuildDir
mv $BuildDir/${Program}*.tar.gz ../output
echo "cleaning up..."
sudo -S rm -r $BuildDir
