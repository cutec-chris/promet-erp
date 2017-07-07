#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-win32/
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
Date=`date`
if [[ "$(uname)" == 'Linux' ]]; then
   platform='linux'
else
   platform='win32'
fi
cat downloads_windows.txt | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > act_downloads.txt
lazbuild ../../source/tools/changewikipage.lpi > build.txt
../../output/$RArchfpc-$platform/changewikipage --mandant=Stora Promet-ERP/dowloadplattforms/windows act_downloads.txt
../../output/$RArchfpc-$platform/changewikipage --mandant=Stora Promet-ERP/changes ../../source/base/changes.txt
../../output/$RArchfpc-$platform/sync_db --mandant=Stora --db=Server3
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
scp -P $AUTOUPLOAD_PORT promet_erp.xml $AUTOUPLOAD_USER@$AUTOUPLOAD_HOST:$AUTOUPLOAD_TARGET
cd $basedir
