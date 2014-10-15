#!/bin/bash
mkdir $2/tools
cp ../../output/$1-linux/tools/sync_db $2/tools
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
mkdir ../executables/$Version/$1
cp $2/tools/sync_db ../executables/$Version/$1
if [ ! -f $2/tools/sync_db ];
then
    exit
fi

