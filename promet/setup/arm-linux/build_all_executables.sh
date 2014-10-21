#!/bin/bash
Params='--cpu='$2' --build-mode=Default'
#if [ "x$2" = "xi386" ]; then
#  Params=$Params' --compiler=/usr/local/lib/fpc/2.7.1/ppc386'
#fi
echo "compiling for $1... $Params"
cd ../../
cd setup/arm-linux
echo "compiling import/exporters..."
echo "compiling sync_..." > scompile-$2-tools.log
lazbuild $Params -q -B ../../source/sync/sync_db.lpi  >> scompile-$2-tools.log
lazbuild $Params -q -B ../../source/scripts/pscript.lpi  >> scompile-$2-tools.log
#lazbuild $Params -q -B ../../source/sync/sync_owncloud.lpi  >> scompile-$2-tools.log
#lazbuild $Params -q -B ../../source/sync/sync_redmine.lpi  >> scompile-$2-tools.log
lazbuild $Params -q -B ../../source/sync/import_document.lpi  >> scompile-$2-tools.log
echo "compiling pop3receiver..." >> scompile-$2-tools.log
lazbuild $Params -q ../../source/sync/pop3receiver.lpi  >> scompile-$2-tools.log
echo "compiling feedreceiver..." >> scompile-$2-tools.log
lazbuild $Params -q ../../source/sync/feedreceiver.lpi  >> scompile-$2-tools.log
echo "compiling twitterreceiver..." >> scompile-$2-tools.log
lazbuild $Params -q ../../source/sync/twitterreceiver.lpi  >> scompile-$2-tools.log
echo "compiling smtpsender..." >> scompile-$2-tools.log
lazbuild $Params -q ../../source/sync/smtpsender.lpi  >> scompile-$2-tools.log
grep -w "Error:" scompile-$2-tools.log
echo "compiling tools..."
echo "compiling cmdwizardmandant..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/cmdwizardmandant.lpi  >> scompile-$2-tools.log
echo "compiling checkin/out..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/checkin.lpi  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/checkout.lpi  >> scompile-$2-tools.log
echo "compiling processmanager..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/processmanager.lpi  >> scompile-$2-tools.log
echo "compiling processdaemon..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/processdaemon.lpi  >> scompile-$2-tools.log
grep -w "Error:" scompile-$2-tools.log
echo "compiling webservices..."
echo "compiling local_appbase..."  > scompile-$2-web.log
lazbuild $Params -q ../../source/webservers/local_appbase.lpi  >> scompile-$2-web.log
echo "compiling imapserver..."  >> scompile-$2-web.log
lazbuild $Params -q ../../source/webservers/imapserver.lpi  >> scompile-$2-web.log
echo "compiling mta..."  >> scompile-$2-web.log
lazbuild $Params -q ../../source/webservers/mta.lpi  >> scompile-$2-web.log
echo "compiling nntpserver..."  >> scompile-$2-web.log
lazbuild $Params -q ../../source/webservers/nntpserver.lpi  >> scompile-$2-web.log
echo "compiling httpserver..."  >> scompile-$2-web.log
lazbuild $Params -q ../../source/webservers/httpserver.lpi  >> scompile-$2-web.log
echo "compiling webdavserver..."  >> scompile-$2-web.log
lazbuild $Params -q ../../source/webservers/webdavserver.lpi  >> scompile-$2-web.log
echo "compiling syslogserver..."  >> scompile-$2-web.log
lazbuild $Params -q ../../source/webservers/syslog.lpi  >> scompile-$2-web.log
grep -w "Error:" scompile-$2-web.log