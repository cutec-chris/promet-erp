#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-linux
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
if [ "x$1" == "x" ]; then
  Arch=`dpkg --print-architecture`
  Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
else
  Arch=$1
  Archfpc=$2
fi
Date=`date`

cat ./downloads_linux_arm.txt | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/CREATEDDATE/$Date/g" \
      -e "s/ARCH/$Arch/g" \
  > act_downloads_linux.txt
echo Promet-ERP/dowloadplattforms/linux-$Archfpc
../../output/$RArchfpc-linux/changewikipage --mandant=Stora Promet-ERP/dowloadplattforms/linux-$Archfpc act_downloads_linux.txt
../../output/$RArchfpc-linux/changewikipage --mandant=Stora Promet-ERP/changes ../../source/base/changes.txt
../../output/$RArchfpc-linux/sync_db --mandant=Stora --db=Server3
cd $basedir