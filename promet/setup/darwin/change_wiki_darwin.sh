#!/bin/bash
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
Arch=`dpkg --print-architecture`
Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
Date=`date`
WinSize=$(ls -s --block-size=1048576 "../output/promet-erp_$(echo $Version)_i386-win32.exe" | cut -d' ' -f1)

cat downloads_darwin.txt | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/DARWIN_SIZE/$WinSize Mb/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > act_downloads.txt
../../output/x86_64-linux/changewikipage --mandant=Stora Promet-ERP/dowloadplattforms/osx act_downloads.txt
../../output/x86_64-linux/tools/sync_db --mandant=Stora
