#!/bin/bash
set -e
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

