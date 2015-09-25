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

# path to lazbuild
export lazbuild=$(which lazbuild)

# Set up widgetset
# Set up processor architecture: i386 or x86_64
if [ $2 ]
  then export lcl=$2
fi
if [ $lcl ] && [ $CPU_TARGET ]
  then export BUILD_ARCH=$(echo "--widgetset=$lcl")" "$(echo "--cpu=$CPU_TARGET")
elif [ $lcl ]
  then export BUILD_ARCH=$(echo "--widgetset=$lcl")
elif [ $CPU_TARGET ]
  then export BUILD_ARCH=$(echo "--cpu=$CPU_TARGET")
fi

build_default()
{
  source/promet.erp/build.sh
}

build_beta()
{
  components/build.sh
  plugins/build.sh
  
  # Build Double Commander
  $lazbuild src/doublecmd.lpi --bm=beta $DC_ARCH
  
  # Build Dwarf LineInfo Extractor
  $lazbuild tools/extractdwrflnfo.lpi
  
  # Extract debug line info
  chmod a+x tools/extractdwrflnfo
  if [ -f doublecmd.dSYM/Contents/Resources/DWARF/doublecmd ]; then
    mv -f doublecmd.dSYM/Contents/Resources/DWARF/doublecmd $(pwd)/doublecmd.dbg
  fi
  tools/extractdwrflnfo doublecmd.dbg
  
  # Strip debug info
  strip doublecmd
}

build_all()
{
  components/build.sh
  plugins/build.sh
  build_default
}


case $1 in
  components)  components/build.sh;;
     plugins)  plugins/build.sh;;
         all)  build_all;;
           *)  build_default;;
esac
