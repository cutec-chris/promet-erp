#!/bin/bash
mkdir $2/web
cp ../../output/$1-linux/local_appbase $2/web
cp ../../output/$1-linux/imapserver $2/web
cp ../../output/$1-linux/mta $2/web
cp ../../output/$1-linux/nntpserver $2/web
cp ../../output/$1-linux/svnserver $2/web
cp ../../output/$1-linux/httpserver $2/web
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
cp $2/local_appbase ../executables/$Version/$1
cp $2/imapserver ../executables/$Version/$1
cp $2/mta ../executables/$Version/$1
cp $2/nntpserver ../executables/$Version/$1
if [ ! -f $2/local_appbase ];
then
    exit
fi