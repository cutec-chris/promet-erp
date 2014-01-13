#!/bin/bash
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');

mkdir $2/tools
install ../../output/$1-linux/prometerp $2
install ../../output/$1-linux/wizardmandant $2
install ../../output/$1-linux/pstarter $2
install ../../output/$1-linux/tools/processmanager $2/tools
install ../../output/$1-linux/tools/messagemanager $2/tools
install ../../output/$1-linux/helpviewer $2
install ../help/help.db $2
install ../../output/$1-linux/tools/*receiver $2/tools
install ../../output/$1-linux/tools/*sender $2/tools
cp ../../output/$1-linux/cdmenue ../executables/$Version/$1
mkdir ../executables/$Version
mkdir ../executables/$Version/$1
cp $2/prometerp ../executables/$Version/$1
cp $2/wizardmandant ../executables/$Version/$1
cp $2/helpviewer ../executables/$Version/$1
cp $2/tools/*receiver ../executables/$Version/$1
cp $2/tools/*sender ../executables/$Version/$1
if [ ! -f ../../output/$1-linux/tools/cdmenue ];
then
    exit
fi

