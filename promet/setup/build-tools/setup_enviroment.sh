#!/bin/bash
# path to lazbuild

if [ -e promet/source/base/version.inc ]
  then
  export lazbuild="lazbuild"
  export grep="grep"
  export TARGET_CPU=$( lazbuild -? | grep 'powerpc_64' | sed 's/.*: \([^ ]\+\)$/\1/')
  export TARGET_WIDGETSET=$( lazbuild -? | grep 'Carbon.' | sed 's/.*: \([^ ]\+\)$/\1/')
  export TARGET_OS=$( lazbuild -? | grep 'linux.' | sed 's/.*: \([^ ]\+\)$/\1/')
  if [ $TEMP ]
    then export BUILD_DIR=$TEMP/promet-build
    else export BUILD_DIR=/tmp/promet-build
  fi
  if [ $TARGET_OS="win32" ]
    then $TARGET_EXTENSION=".exe"
  fi
  Year=`date +%y`
  Month=`date +%m`
  Day=`date +%d`
  export BUILD_DATE=20$Year$Month$Day
  Version=$(sed 's/\r//g' promet/source/base/version.inc).$(sed 's/\r//g' promet/source/base/revision.inc)
  export BUILD_VERSION=$(echo $Version | sed 's/\n//g');
  echo "Version to Build:$BUILD_VERSION"
  export OUTPUT_DIR=promet/setup/output/$BUILD_VERSION
  # Set up widgetset
  # Set up processor architecture: i386 or x86_64
  if [ $2 ]
    then export lcl=$2
  fi
  if [ $TARGET_WIDGETSET ] && [ $TARGET_CPU ]
    then export BUILD_ARCH=$(echo "--widgetset=$TARGET_WIDGETSET")" "$(echo "--cpu=$TARGET_CPU")
  elif [ $TARGET_WIDGETSET ]
    then export BUILD_ARCH=$(echo "--widgetset=$TARGET_WIDGETSET")
  elif [ $TARGET_CPU ]
    then export BUILD_ARCH=$(echo "--cpu=$TARGET_CPU")
  fi
  export BUILD_PARAMS=-q
fi

