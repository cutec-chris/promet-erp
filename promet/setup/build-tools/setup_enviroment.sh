#!/bin/bash
# path to lazbuild

if [ -e promet/source/base/version.inc ]
  then
  export lazbuild="lazbuild"
  export grep="grep"
  export SED="sed"
  $SED --version > /dev/null
  if [ "$?" -ne "0" ]; then
    export SED="$(PWD)/promet/setup/build-tools/sed.exe"
  fi
  export TARGET_CPU=$( lazbuild -? | grep 'powerpc_64' | $SED 's/.*: \([^ ]\+\)$/\1/')
  export TARGET_WIDGETSET=$( lazbuild -? | grep 'Carbon.' | $SED 's/.*: \([^ ]\+\)$/\1/')
  export TARGET_OS=$( lazbuild -? | grep 'linux.' | $SED 's/.*: \([^ ]\+\)$/\1/')
  if [ "x$TARGET_OS" = "xwin32" ]; then
    export TARGET_EXTENSION='.exe'
    else
    export TARGET_EXTENSION=''
  fi
  if [ $TEMP ]
    then export BUILD_DIR=$TEMP/promet-build
    else export BUILD_DIR=/tmp/promet-build
  fi
  Year=`date +%y`
  Month=`date +%m`
  Day=`date +%d`
  export BUILD_DATE=20$Year$Month$Day
  Version=$($SED 's/\r//g' promet/source/base/version.inc).$($SED 's/\r//g' promet/source/base/revision.inc)
  export BUILD_VERSION=$(echo $Version | $SED 's/\n//g');
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

