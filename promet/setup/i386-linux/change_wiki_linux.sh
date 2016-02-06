#!/bin/bash
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
Arch=`dpkg --print-architecture`
Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
Date=`date`

cat downloads_linux.txt | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > act_downloads_linux.txt
../../output/x86_64-linux/changewikipage --mandant=Stora Promet-ERP/dowloadplattforms/linux-$TARGET_CPU act_downloads_linux.txt
../../output/x86_64-linux/changewikipage --mandant=Stora Promet-ERP/changes ../source/base/changes.txt
../../output/x86_64-linux/tools/sync_db --mandant=Stora