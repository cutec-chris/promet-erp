#!/bin/bash
# You can execute this script with different parameters:
# default - compiling promet basic programms
# server - compiling promet server programms
# components - compiling components needed
# plugins - compiling plugins
# tools - compiling commandline plugins
# web - compiling webserver
# imap - compiling imapserver
# dav - compiling dav server
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
  . ./promet/source/plugins/build.sh
  . ./promet/source/tools/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/tools/build_visual.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/messagemanager/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/promet.erp/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/help/build.sh
  . ./promet/importdata/build.sh
}

build_server()
{
  . ./promet/source/tools/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/webservers/build_imap.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/webservers/build_dav.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/sync/build_mail.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/sync/build_feed.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/sync/build_mqtt.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/sync/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/webservers/build_webserver.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/webservers/build_xmpp.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/scripts/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
}

build_all()
{
  echo "Building all..."
  . ./promet/source/components/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/testcases/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/plugins/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/tools/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/tools/build_visual.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/messagemanager/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/promet.erp/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/meeting/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/tools/build_archivestore.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/statistics/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/webservers/build_webserver.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/webservers/build_imap.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/webservers/build_dav.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/sync/build_mail.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/sync/build_feed.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/sync/build_mqtt.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/source/sync/build.sh
  if [ "$?" -ne "0" ]; then
    exit 1
  fi
  . ./promet/help/build.sh
  . ./promet/importdata/build.sh
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
  rm -f -r promet/general/lib
  find promet/source -type f -iname "*.compiled" -exec rm -f {} \;
  find promet/source -type f -iname "*.bak" -exec rm -f {} \;
  find promet/source -type f -iname "*.ppu" -exec rm -f {} \;
  find promet/source -type f -iname "*.o" -exec rm -f {} \;
  find promet/source -type f -iname "*.or" -exec rm -f {} \;
  find promet/source -type f -iname "*.rst" -exec rm -f {} \;
}


case $1 in
       tests)  . ./promet/source/testcases/build.sh;;
  components)  . ./promet/source/components/build.sh;;
     plugins)  $BASH promet/source/plugins/build.sh;;
       tools)  . ./promet/source/tools/build.sh;;
tools-visual)  . ./promet/source/tools/build_visual.sh;;
  importdata)  . ./promet/importdata/build.sh;;
        help)  . ./promet/help/build.sh;;
      promet)  . ./promet/source/promet.erp/build.sh;;
  messageman)  . ./promet/source/messagemanager/build.sh;;
     meeting)  . ./promet/source/meeting/build.sh;;
     archive)  . ./promet/source/tools/build_archivestore.sh;;
  statistics)  . ./promet/source/statistics/build.sh;;
         web)  . ./promet/source/webservers/build_webserver.sh;;
        imap)  . ./promet/source/webservers/build_imap.sh;;
        xmpp)  . ./promet/source/webservers/build_xmpp.sh;;
         dav)  . ./promet/source/webservers/build_dav.sh;;
        mail)  . ./promet/source/sync/build_mail.sh;;
        feed)  . ./promet/source/sync/build_feed.sh;;
        mqtt)  . ./promet/source/sync/build_mqtt.sh;;
        sync)  . ./promet/source/sync/build.sh;;
       clean)  clean_all;;
      server)  build_server;;
         all)  build_all;;
           *)  build_default;;
esac
