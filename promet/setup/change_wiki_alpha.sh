#!/bin/bash
basedir=$(pwd)
cd $(dirname "$0")
Version=$(sed 's/\r//g' ../source/base/version.inc).$(sed 's/\r//g' ../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
Arch=`dpkg --print-architecture`
Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
if [ "x$Archfpc" = "x" ]; then
  Archfpc=x86_64
fi
Date=`date`
if [ "x$STORA_CONN" != "x" ]; then
  mkdir ~/.prometerp
  echo "SQL" > ~/.prometerp/Stora.perml
  echo $STORA_CONN >> ~/.prometerp/Stora.perml
fi

echo Promet-ERP/dowloadplattforms/linux-$Archfpc
../output/$Archfpc-linux/changewikipage --mandant=Stora /tmp/act_alphadownload.txt
../output/$Archfpc-linux/changewikipage --mandant=Stora Promet-ERP/changes ../source/base/changes.txt
../output/$Archfpc-linux/sync_db --mandant=Stora --db=Help
../output/$Archfpc-linux/sync_db --mandant=Stora --db=Server3
cd $basedir