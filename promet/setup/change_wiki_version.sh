#!/bin/bash
basedir=$(pwd)
. promet/setup/build-tools/setup_enviroment.sh
. promet/setup/build-tools/doupload.sh x x
cd $(dirname "$0")
cd ..
cd ..
if [ "x$STORA_CONN" != "x" ]; then
  mkdir ~/.prometerp
  echo "SQL" > ~/.prometerp/Stora.perml
  echo $STORA_CONN >> ~/.prometerp/Stora.perml
fi
RArchfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
if [ "x$RArchfpc" = "x" ]; then
  RArchfpc=x86_64
fi
. ./promet/setup/i386-linux/change_wiki_linux.sh i386 i386
. ./promet/setup/i386-linux/change_wiki_linux.sh amd64 amd64
. ./promet/setup/i386-linux/change_wiki_arm_linux.sh arm arm
. ./promet/setup/i386-win32/change_wiki_windows.sh
. ./promet/setup/portableapps/change_wiki.sh

set_current()
{
  targetfile=$1_i386-win32-$BUILD_VERSION.zip
  targetcur=$1_i386-win32-current.zip
  ssh $AUTOUPLOAD_USER@$AUTOUPLOAD_HOST -p $AUTOUPLOAD_PORT "cd $AUTOUPLOAD_TARGET;ln -s -f $targetfile $targetcur"
}

set_current avad;
set_current feedreceiver;
set_current fhemreceiver;
set_current imapserver;
set_current mailreceiver;
set_current meeting;
set_current messagemanager;
set_current mqttreceiver;
set_current mysqlclient;
set_current plugins;
set_current postgresclient;
set_current prometerp;
set_current pscript;
set_current sqliteclient;
set_current statistics;
set_current sync;
set_current tools;
set_current visualtools;
set_current win32tools;
set_current xmpp;



cd $basedir