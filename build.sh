#!/bin/bash
set -e
# You can execute this script with different parameters:
# components - compiling components needed
# plugins - compiling plugins
# tools - compiling commandline plugins
# web - compiling webserver
# imap - compiling imapserver
# dav - compiling dav server
# all - compiling all
# default - compiling program only (using by default)
$BASH promet/setup/build-tools/setup_enviroment.sh

build_default()
{
  echo "Building default..."
  echo $BUILD_ARCH
}

build_all()
{
  echo "Building all..."
}


case $1 in
  components)  components/build.sh;;
     plugins)  plugins/build.sh;;
         all)  build_all;;
           *)  build_default;;
esac
