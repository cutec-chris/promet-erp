#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-linux
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
Arch=`dpkg --print-architecture`
Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
Date=`date`

cat ./downloads_linux.txt | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/CREATEDDATE/$Date/g" \
      -e "s/ARCH/$Arch/g" \
  > act_downloads_linux.txt
lazbuild ../../source/tools/changewikipage.lpi
echo Promet-ERP/dowloadplattforms/linux-$Archfpc
../../output/x86_64-linux/tools/changewikipage --mandant=Stora Promet-ERP/dowloadplattforms/linux-$Archfpc act_downloads_linux.txt
../../output/x86_64-linux/tools/changewikipage --mandant=Stora Promet-ERP/changes ../../source/base/changes.txt
../../output/x86_64-linux/tools/sync_db --mandant=Stora
cd $basedir