#!/bin/bash
# You can execute this script with different parameters:
# default - compiling promet basic programms
# server - compiling promet server programms
# components - compiling components needed
# plugins - compiling plugins
# tools - compiling commandline plugins
# imap - compiling imapserver
# xmpp - compiling xmpp messaging interface
# mail - send/reveive mail components
# feed - reveive feed and social components
# mqtt - receive messages from mqtt brooker
# sync - sync_db
# archive - archivestore program to extract archived files
# statistics - the repoting and statistics tool
# help - cleanup help.db from test data and zip it
# importdata - Zip the standard Importdata
# all - compiling all
# default - compiling program only (using by default)
cd "$0"
ls
. ./promet/setup/build-tools/setup_enviroment.sh

if [ -d $BUILD_DIR ]
  then
    echo "."
  else
    mkdir -p $BUILD_DIR
fi
if [ -d $OUTPUT_DIR ]
  then
    echo "."
  else
    mkdir -p $OUTPUT_DIR
fi

build_default()
{
  echo "Building default..."
  . ./promet/source/plugins/build.sh $2
  . ./promet/source/tools/build.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/tools/build_visual.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/messagemanager/build.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/promet.erp/build.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/production/build.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/help/build.sh
  . ./promet/importdata/build.sh $2
}

build_server()
{
  . ./promet/source/tools/build.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  . ./promet/source/webservers/build_imap.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  . ./promet/source/sync/build_mail.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  . ./promet/source/sync/build_feed.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  . ./promet/source/sync/build_mqtt.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  . ./promet/source/sync/build_fhem.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  . ./promet/source/sync/build.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  . ./promet/source/webservers/build_xmpp.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  . ./promet/source/scripts/build.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  if [ "x$TARGET_OS" = "xlinux" ]; then
    . ./promet/setup/i386-linux/build_server.sh $2
  fi
  sleep 2
}

build_all()
{
  echo "Building all..."
  . ./promet/help/build.sh "$2"
  build_server $1 $2;
  sleep 2
  build_default $1 $2;
  . ./promet/source/meeting/build.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  . ./promet/source/tools/build_archivestore.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  . ./promet/source/statistics/build.sh $2
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  sleep 2
  if [ "x$TARGET_OS" != "xwin32" ]; then
    . ./promet/importdata/build.sh "$2"
  fi
  sleep 2
  if [ "x$TARGET_OS" = "xwin32" ]; then
    . ./promet/setup/i386-win32/build.sh $2
    . ./promet/setup/portableapps/build.sh $2
  fi
  sleep 2
  if [ "x$TARGET_OS" = "xlinux" ]; then
    . ./promet/setup/i386-linux/build.sh $2
  fi
}

upload()
{
  if [ "x$TARGET_OS" = "xwin32" ]; then
    . ./promet/setup/i386-win32/upload.sh
  fi
  if [ "x$TARGET_OS" = "xlinux" ]; then
    . ./promet/setup/i386-linux/upload.sh
  fi
}

clean_all()
{
  rm -f -r $OUTPUT_DIR
  mkdir -p $OUTPUT_DIR
  rm -f -r promet/source/base/dbintfs/lib
  rm -f -r promet/source/base/dbintfs/base_scripts/lib
  rm -f -r promet/source/base/dbintfs/base_pm/lib
  rm -f -r promet/source/promet.erp/lib
  rm -f -r promet/source/tools/lib
  rm -f -r promet/source/projectmanagement/lib
  rm -f -r promet/source/clientmanagement/lib
  rm -f -r promet/source/messageimport/lib
  rm -f -r promet/source/archivestore/lib
  rm -f -r promet/source/base/base_calendar/lib
  rm -f -r promet/source/base/base_documents/lib
  rm -f -r promet/source/base_erp/lib
  rm -f -r promet/source/base/base_messaging/lib
  rm -f -r promet/source/base/base_ocr/lib
  rm -f -r promet/source/base/base_office/lib
  rm -f -r promet/source/base_phone/lib
  rm -f -r promet/source/base_pm/lib
  rm -f -r promet/source/base_repair/lib
  rm -f -r promet/source/base_wiki/lib
  rm -f -r promet/source/base_help/lib
  rm -f -r promet/source/base/base_frames/lib
  rm -f -r promet/source/base/base_forms/lib
  rm -f -r promet/source/base/base_inet/lib
  rm -f -r promet/source/base/base_docmanage/lib
  rm -f -r promet/source/meeting/lib
  rm -f -r promet/source/testcases/lib
  rm -f -r promet/source/webstat/lib
  rm -f -r promet/source/checkout/lib
  rm -f -r promet/source/webservers/lib
  rm -f -r promet/source/import/lib
  rm -f -r promet/source/pos/lib
  rm -f -r promet/source/statistics/lib
  rm -f -r promet/source/sync/lib
  rm -f -r promet/source/timeregistering/lib
  rm -f -r promet/source/tools/lib
  rm -f -r promet/source/components/zeos/packages/lazarus/lib
  rm -f -r promet/general/lib
  find promet/source -type f -iname "*.compiled" -exec rm -f {} \;
  find promet/source -type f -iname "*.bak" -exec rm -f {} \;
  find promet/source -type f -iname "*.ppu" -exec rm -f {} \;
  find promet/source -type f -iname "*.o" -exec rm -f {} \;
  find promet/source -type f -iname "*.or" -exec rm -f {} \;
  find promet/source -type f -iname "*.rst" -exec rm -f {} \;
}


case $1 in
       tests)  . ./promet/source/testcases/build.sh $2;;
  components)  . ./promet/source/components/build.sh $2;;
     plugins)  . ./promet/source/plugins/build.sh $2;;
       tools)  . ./promet/source/tools/build.sh $2;;
      visual)  . ./promet/source/tools/build_visual.sh $2;;
  importdata)  . ./promet/importdata/build.sh $2;;
        help)  . ./promet/help/build.sh $2;;
      promet)  . ./promet/source/promet.erp/build.sh $2;;
  messageman)  . ./promet/source/messagemanager/build.sh $2;;
     meeting)  . ./promet/source/meeting/build.sh $2;;
     archive)  . ./promet/source/tools/build_archivestore.sh $2;;
  statistics)  . ./promet/source/statistics/build.sh $2;;
        imap)  . ./promet/source/webservers/build_imap.sh $2;;
        xmpp)  . ./promet/source/webservers/build_xmpp.sh $2;;
        mail)  . ./promet/source/sync/build_mail.sh $2;;
        feed)  . ./promet/source/sync/build_feed.sh $2;;
        mqtt)  . ./promet/source/sync/build_mqtt.sh $2;;
        fhem)  . ./promet/source/sync/build_fhem.sh $2;;
        sync)  . ./promet/source/sync/build.sh $2;;
  winclients)  . ./promet/setup/i386-win32/build.sh $2;;
    portable)  . ./promet/setup/portableapps/build.sh $2;;
  linclients)  . ./promet/setup/i386-linux/build.sh $2;;
      upload)  upload;;
       clean)  clean_all;;
      server)  build_server $1 $2;;
         all)  build_all $1 $2;;
           *)  build_default $1 $2;;
esac
