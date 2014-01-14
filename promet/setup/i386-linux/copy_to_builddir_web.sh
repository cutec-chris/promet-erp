#!/bin/bash
mkdir $2/web
mkdir $2/tools
cp ../../output/$1-linux/tools/cmdwizardmandant $2/tools/
cp ../../output/$1-linux/tools/processmanager $2/tools/
cp ../../output/$1-linux/tools/processdaemon $2/tools/
cp ../../output/$1-linux/web/local_appbase $2/web
cp ../../output/$1-linux/web/imapserver $2/web
cp ../../output/$1-linux/web/mta $2/web
cp ../../output/$1-linux/web/nntpserver $2/web
cp ../../output/$1-linux/web/svnserver $2/web
cp ../../output/$1-linux/web/httpserver $2/web
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
cp $2/web/local_appbase ../executables/$Version/$1
cp $2/web/imapserver ../executables/$Version/$1
cp $2/web/mta ../executables/$Version/$1
cp $2/web/nntpserver ../executables/$Version/$1
cp $2/web/svnserver ../executables/$Version/$1
cp $2/tools/processmanager ../executables/$Version/$1
cp $2/tools/processdaemon ../executables/$Version/$1
cp $2/tools/cmdwizardmandant ../executables/$Version/$1