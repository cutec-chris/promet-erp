#!/bin/bash
# You can execute this script with different parameters:
# components - compiling components needed
# plugins - compiling plugins
# tools - compiling commandline plugins
# web - compiling webserver
# imap - compiling imapserver
# dav - compiling dav server
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
    mkdir $BUILD_DIR
fi
if [ -d $OUTPUT_DIR ]
  then
    echo "."
  else
    mkdir $OUTPUT_DIR
fi

build_default()
{
  echo "Building default..."
  . ./promet/source/plugins/build.sh
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
         dav)  . ./promet/source/webservers/build_dav.sh;;
        mail)  . ./promet/source/sync/build_mail.sh;;
        feed)  . ./promet/source/sync/build_feed.sh;;
        mqtt)  . ./promet/source/sync/build_mqtt.sh;;
        sync)  . ./promet/source/sync/build.sh;;
         all)  build_all;;
           *)  build_default;;
esac
