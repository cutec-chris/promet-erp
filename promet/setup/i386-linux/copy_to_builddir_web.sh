#!/bin/bash
mkdir $2/web
mkdir $2/tools
cp ../../output/$1-linux/cmdwizardmandant $2/tools/
cp ../../output/$1-linux/tools/processmanager $2/tools/
cp ../../output/$1-linux/tools/processdaemon $2/tools/promet-process
cp ../../output/$1-linux/tools/sync_* $2/tools/
cp ../../output/$1-linux/tools/syslog $2/tools
cp ../../output/$1-linux/tools/webserver $2/tools
cp ../../output/$1-linux/tools/pscript $2/tools
rm $2/tools/sync_*.dbg
cp ../../output/$1-linux/tools/import_* $2/tools/
rm $2/tools/import_*.dbg
cp ../../output/$1-linux/tools/imapserver $2/tools
cp ../../output/$1-linux/tools/mta $2/tools
cp ../../output/$1-linux/tools/nntpserver $2/tools
cp ../../output/$1-linux/tools/webdavserver $2/tools
cp ../../output/$1-linux/tools/httpserver $2/tools
cp ../../output/$1-linux/tools/message_xmpp $2/tools
install ../../output/$1-linux/tools/*receiver $2/tools
install ../../output/$1-linux/tools/*sender $2/tools
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
cp $2/tools/webserver ../executables/$Version/$1
cp $2/tools/imapserver ../executables/$Version/$1
cp $2/tools/mta ../executables/$Version/$1
cp $2/tools/nntpserver ../executables/$Version/$1
cp $2/tools/webdavserver ../executables/$Version/$1
cp $2/tools/processmanager ../executables/$Version/$1
cp $2/tools/promet-process ../executables/$Version/$1
cp $2/tools/syslog ../executables/$Version/$1
cp $2/tools/cmdwizardmandant ../executables/$Version/$1
cp $2/tools/*receiver ../executables/$Version/$1
cp $2/tools/*sender ../executables/$Version/$1
cp $2/tools/sync_* ../executables/$Version/$1
cp $2/tools/import_* ../executables/$Version/$1
cp $2/tools/message_* ../executables/$Version/$1
