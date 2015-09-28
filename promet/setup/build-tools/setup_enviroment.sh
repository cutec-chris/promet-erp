#!/bin/bash
# path to lazbuild

export lazbuild="lazbuild"
export grep="grep"
Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
Archlazbuild=$(lazbuild -? | grep "powerpc_64" | sed 's/.*: \([^ ]\+\)$/\1/')
Widgetsetlazbuild=$(lazbuild -? | grep "Carbon." | sed 's/.*: \([^ ]\+\)$/\1/')
OSlazbuild=$(lazbuild -? | grep "linux." | sed 's/.*: \([^ ]\+\)$/\1/')

export TARGET_OS=$OSlazbuild
export TARGET_CPU=$Archlazbuild
export TARGET_WIDGETSET=$Widgetsetlazbuild

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
export BUILD_PARAMS=-qq
