#!/bin/bash
echo "compiling for $1..."
cd ../../
lazbuild --add-package $(pwd)/source/components/promet_components.lpk
lazbuild --add-package $(pwd)/source/base_help/phelp.lpk
cd setup/$2-linux
echo "compiling apps..."
echo "compiling messagemanager..." > scompile-$2.log
lazbuild -q -B -r ../../source/messagemanager/messagemanager.lpi >> scompile-$2.log
echo "compiling promet..." >> scompile-$2.log
lazbuild -q -B ../../source/promet.erp/prometerp.lpi > scompile-$2.log
echo "compiling statistics..." >> scompile-$2.log
lazbuild -q -B -r ../../source/statistics/statistics.lpi  >> scompile-$2.log
echo "compiling wizardmandant..." >> scompile-$2.log
lazbuild -q -B ../../source/tools/wizardmandant.lpi  >> scompile-$2.log
echo "compiling import/exporters..."
echo "compiling sync_db..." >> scompile-$2.log
lazbuild -q -B ../../source/sync/sync_db.lpi  >> scompile-$2.log
echo "compiling pop3receiver..." >> scompile-$2.log
lazbuild -q -B ../../source/messageimport/pop3receiver.lpi  >> scompile-$2.log
echo "compiling feedreceiver..." >> scompile-$2.log
lazbuild -q -B ../../source/messageimport/feedreceiver.lpi  >> scompile-$2.log
echo "compiling twitterreceiver..." >> scompile-$2.log
lazbuild -q -B ../../source/messageimport/twitterreceiver.lpi  >> scompile-$2.log
echo "compiling smtpsender..." >> scompile-$2.log
lazbuild -q -B ../../source/messageimport/smtpsender.lpi  >> scompile-$2.log
echo "compiling tools..."
echo "compiling pstarter..." >> scompile-$2.log
lazbuild -q -B ../../source/tools/pstarter.lpi  >> scompile-$2.log
echo "compiling cdmenue..."  >> scompile-$2.log
lazbuild -q -B ../../source/tools/cdmenue.lpi  >> scompile-$2.log
echo "compiling cmdwizardmandant..."  >> scompile-$2.log
lazbuild -q -B -r ../../source/tools/cmdwizardmandant.lpi  >> scompile-$2.log
echo "compiling checkin/out..."  >> scompile-$2.log
lazbuild -q -B ../../source/tools/checkin.lpi  >> scompile-$2.log
lazbuild -q -B ../../source/tools/checkout.lpi  >> scompile-$2.log
lazbuild -q -B ../../source/tools/tableedit.lpi  >> scompile-$2.log
echo "compiling archivestore..."  >> scompile-$2.log
lazbuild -q -B ../../source/tools/archivestore.lpi  >> scompile-$2.log
echo "compiling clientmanagement..."  >> scompile-$2.log
lazbuild -q -B ../../source/tools/clientmanagement.lpi  >> scompile-$2.log
echo "compiling processmanager..."  >> scompile-$2.log
lazbuild -q -B ../../source/tools/processmanager.lpi  >> scompile-$2.log
echo "compiling helpviewer..."  >> scompile-$2.log
lazbuild -q -B ../../source/tools/helpviewer.lpi  >> scompile-$2.log

echo "compiling webservices..."
echo "compiling local_appbase..."  >> scompile-$2.log
lazbuild -q -B ../../source/webservers/local_appbase.lpi  >> scompile-$2.log
echo "compiling imapserver..."  >> scompile-$2.log
lazbuild -q -B ../../source/webservers/imapserver.lpi  >> scompile-$2.log
echo "compiling mta..."  >> scompile-$2.log
lazbuild -q -B ../../source/webservers/mta.lpi  >> scompile-$2.log
echo "compiling nntpserver..."  >> scompile-$2.log
lazbuild -q -B ../../source/webservers/nntpserver.lpi  >> scompile-$2.log
echo "compiling svnserver..."  >> scompile-$2.log
lazbuild -q -B ../../source/webservers/svnserver.lpi  >> scompile-$2.log
echo "compiling httpserver..."  >> scompile-$2.log
lazbuild -q -B ../../source/webservers/httpserver.lpi  >> scompile-$2.log
