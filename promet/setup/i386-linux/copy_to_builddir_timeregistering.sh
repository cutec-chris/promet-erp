#!/bin/bash
mkdir $2/tools
cp ../../output/$1-linux/timeregistering $2
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
cp $2/timeregistering ../executables/$Version/$1
if [ ! -f $2/timeregistering ];
then
    exit
fi
