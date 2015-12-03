#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-win32
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
Arch=`dpkg --print-architecture`
Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
Date=`date`
cat downloads_windows.txt | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > act_downloads.txt
#../../output/x86_64-linux/tools/changewikipage --mandant=Stora Promet-ERP/dowloadplattforms/windows act_downloads.txt
#../../output/x86_64-linux/tools/changewikipage --mandant=Stora Promet-ERP/changes ../../source/base/changes.txt
#../../output/x86_64-linux/tools/sync_db --mandant=Stora
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`
cat ../promet_erp_clean.xml | \
  sed -e "s/PROGVERSION/$Version/g" \
      -e "s/CREATEDDATE/$Date/g" \
      -e "s/YEAR/20$Year/g" \
      -e "s/MONTH/$Month/g" \
      -e "s/DAY/$Day/g" \
  > promet_erp.xml
scp -P 232 promet_erp.xml autoupload@ullihome.de:promet_upload_target
cd $basedir