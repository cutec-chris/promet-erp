#!/bin/bash
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
Arch=`dpkg --print-architecture`
Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
Date=`date`
WinSize=$(ls -s --block-size=1048576 "../output/promet-erp_$(echo $Version)_i386-win32.exe" | cut -d' ' -f1)
WinToolsSize=$(ls -s --block-size=1048576 "../output/promet-erp-tools_$(echo $Version)_i386-win32.exe" | cut -d' ' -f1)
WinTRSize=$(ls -s --block-size=1048576 "../output/promet-erp-timeregistering_$(echo $Version)_i386-win32.exe" | cut -d' ' -f1)
WinPDBSize=$(ls -s --block-size=1048576 "../output/db_setup_postgres.exe" | cut -d' ' -f1)
WinSizeK=$(ls -s --block-size=1024 "../output/promet-erp_$(echo $Version)_i386-win32.exe" | cut -d' ' -f1)
WinSizeB=$(ls -s --block-size=1 "../output/promet-erp_$(echo $Version)_i386-win32.exe" | cut -d' ' -f1)

cat downloads_windows.txt | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/WIN_SIZE/$WinSize Mb/g" \
      -e "s/WIN_TOOLS_SIZE/$WinToolsSize Mb/g" \
      -e "s/WIN_TR_SIZE/$WinTRSize Mb/g" \
      -e "s/WIN_PDB_SIZE/$WinPDBSize Mb/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > act_downloads.txt
../../output/x86_64-linux/changewikipage --mandant=Stora Promet-ERP/dowloadplattforms/windows act_downloads.txt
../../output/x86_64-linux/changewikipage --mandant=Stora Promet-ERP/changes ../../source/base/changes.txt
../../output/x86_64-linux/tools/sync_db --mandant=Stora
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`
File="Promet-ERP_$(echo $Version)_i386-win32.exe"
cat ../promet_erp_clean.xml | \
  sed -e "s/PROGVERSION/$Version/g" \
      -e "s/WIN_SIZEB/$WinSizeB/g" \
      -e "s/WIN_SIZEK/$WinSizeK/g" \
      -e "s/WIN_SIZE/$WinSize/g" \
      -e "s/CREATEDDATE/$Date/g" \
      -e "s/YEAR/20$Year/g" \
      -e "s/MONTH/$Month/g" \
      -e "s/DAY/$Day/g" \
  > promet_erp.xml
scp -P 232 promet_erp.xml autoupload@ullihome.de:promet_upload_target