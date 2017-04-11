#!/bin/bash
# path to lazbuild
if [ "$(expr substr $(uname -s) 1 5)" == "MINGW" ]; then
    export TARGET_EXTENSION='.exe'
  else
    export TARGET_EXTENSION=''
fi
if [ "x$lazbuild" = "x" ]
  then
  export lazbuild="lazbuild$TARGET_EXTENSION"
  echo "$lazbuild"
  export grep="grep"
  export SED="sed"
  export SQLITE3="sqlite3"
  $SED 's/||/| |/g' /dev/null
  if [ "$?" -ne "0" ]; then
    export SED="$(PWD)/promet/setup/build-tools/sed.exe"
  fi
  if [ "$(expr substr $(uname -s) 1 5)" == "MINGW" ]; then
    export SQLITE3="$(PWD)/promet/setup/build-tools/sqlite3.exe"
  fi
  export TARGET_CPU=$( $lazbuild -? | grep 'powerpc_64' | $SED -e 's/.*: //')
  export TARGET_WIDGETSET=$( $lazbuild -? | grep 'Carbon.' | $SED 's/.*: //')
  export TARGET_OS=$( $lazbuild -? | grep 'linux.' | $SED -e 's/.*: //')
  echo "CPU:$TARGET_CPU"
  echo "OS:$TARGET_OS"
  echo "Widgetset:$TARGET_WIDGETSET"
  if [ "x$TARGET_CPU" = "xarm" ]; then
    export TARGET_OS='linux'
  fi
  if [ $TEMP ]
    then export BUILD_DIR=$TEMP/promet-build
    else export BUILD_DIR=/tmp/promet-build
  fi
  Year=`date +%y`
  Month=`date +%m`
  Day=`date +%d`
  export BUILD_DATE=20$Year$Month$Day
  echo $(pwd)
  Version=$(cat promet/source/base/version.inc).$(cat promet/source/base/revision.inc)
  export BUILD_VERSION=$(echo $Version);
  if [ "x$TARGET_OS" != "xwin32" ]; then
    export BUILD_VERSION=$(echo $Version | $SED $'s@\r@@g');
  fi
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
  if [ "x$TARGET_CPU" != "xarm" ];
    then export BUILD_PARAMS="-B -q"
    else export BUILD_PARAMS="-B"
  fi
fi
