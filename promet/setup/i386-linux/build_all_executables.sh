#!/bin/bash
echo "compiling for $1..."
cd ../../
lazbuild --add-package $(pwd)/source/components/promet_components.lpk
lazbuild --add-package $(pwd)/source/base_help/phelp.lpk
cd setup/i386-linux
echo "compiling apps..."
echo "compiling messagemanager..." > scompile-$2-apps.log
lazbuild -B ../../source/messagemanager/messagemanager.lpi >> scompile-$2-apps.log
echo "compiling promet..." >> scompile-$2-apps.log
lazbuild -q -B ../../source/promet.erp/prometerp.lpi >> scompile-$2-apps.log
echo "compiling statistics..." >> scompile-$2-apps.log
lazbuild -q  ../../source/statistics/statistics.lpi  >> scompile-$2-apps.log
echo "compiling wizardmandant..." >> scompile-$2-apps.log
lazbuild -q ../../source/tools/wizardmandant.lpi  >> scompile-$2-apps.log
echo "compiling import/exporters..."
echo "compiling sync_db..." > scompile-$2-tools.log
lazbuild -q -B ../../source/sync/sync_db.lpi  >> scompile-$2-tools.log
echo "compiling pop3receiver..." >> scompile-$2-tools.log
lazbuild -q ../../source/messageimport/pop3receiver.lpi  >> scompile-$2-tools.log
echo "compiling feedreceiver..." >> scompile-$2-tools.log
lazbuild -q ../../source/messageimport/feedreceiver.lpi  >> scompile-$2-tools.log
echo "compiling twitterreceiver..." >> scompile-$2-tools.log
lazbuild -q ../../source/messageimport/twitterreceiver.lpi  >> scompile-$2-tools.log
echo "compiling smtpsender..." >> scompile-$2-tools.log
lazbuild -q ../../source/messageimport/smtpsender.lpi  >> scompile-$2-tools.log
echo "compiling tools..."
echo "compiling pstarter..." >> scompile-$2-tools.log
lazbuild -q ../../source/tools/pstarter.lpi  >> scompile-$2-tools.log
echo "compiling cdmenue..."  >> scompile-$2-tools.log
lazbuild -q ../../source/tools/cdmenue.lpi  >> scompile-$2-tools.log
echo "compiling cmdwizardmandant..."  >> scompile-$2-tools.log
lazbuild -q OPT=" -va" ../../source/tools/cmdwizardmandant.lpi  >> scompile-$2-tools.log
echo "compiling checkin/out..."  >> scompile-$2-tools.log
lazbuild -q ../../source/tools/checkin.lpi  >> scompile-$2-tools.log
lazbuild -q ../../source/tools/checkout.lpi  >> scompile-$2-tools.log
lazbuild -q ../../source/tools/tableedit.lpi  >> scompile-$2-tools.log
echo "compiling archivestore..."  >> scompile-$2-tools.log
lazbuild -q ../../source/tools/archivestore.lpi  >> scompile-$2-tools.log
echo "compiling clientmanagement..."  >> scompile-$2-tools.log
lazbuild -q ../../source/tools/clientmanagement.lpi  >> scompile-$2-tools.log
echo "compiling processmanager..."  >> scompile-$2-tools.log
lazbuild -q ../../source/tools/processmanager.lpi  >> scompile-$2-tools.log
echo "compiling processdaemon..."  >> scompile-$2-tools.log
lazbuild -q ../../source/tools/processdaemon.lpi  >> scompile-$2-tools.log
echo "compiling helpviewer..."  >> scompile-$2-tools.log
lazbuild -q ../../source/tools/helpviewer.lpi  >> scompile-$2-tools.log

echo "compiling webservices..."
echo "compiling local_appbase..."  > scompile-$2-web.log
lazbuild -q ../../source/webservers/local_appbase.lpi  >> scompile-$2-web.log
echo "compiling imapserver..."  >> scompile-$2-web.log
lazbuild -q ../../source/webservers/imapserver.lpi  >> scompile-$2-web.log
echo "compiling mta..."  >> scompile-$2-web.log
lazbuild -q ../../source/webservers/mta.lpi  >> scompile-$2-web.log
echo "compiling nntpserver..."  >> scompile-$2-web.log
lazbuild -q ../../source/webservers/nntpserver.lpi  >> scompile-$2-web.log
echo "compiling svnserver..."  >> scompile-$2-web.log
lazbuild -q ../../source/webservers/svnserver.lpi  >> scompile-$2-web.log
echo "compiling httpserver..."  >> scompile-$2-web.log
lazbuild -q ../../source/webservers/httpserver.lpi  >> scompile-$2-web.log
