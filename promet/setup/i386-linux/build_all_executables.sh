#!/bin/bash
Params='--cpu='$2' --build-mode=Default'
#if [ "x$2" = "xi386" ]; then
#  Params=$Params' --compiler=/usr/local/lib/fpc/2.7.1/ppc386'
#fi
echo "compiling for $1... $Params"
cd ../../
lazbuild --add-package $(pwd)/source/base/base_help/phelp.lpk
lazbuild --add-package $(pwd)/source/base/base_frames/pvisualframes.lpk
lazbuild --add-package $(pwd)/source/base/base_docmanage/pdocmanage.lpk
lazbuild --add-package $(pwd)/source/base/base_forms/pvisualforms.lpk
lazbuild --add-package $(pwd)/source/base/base_wiki/rtfconvert_pkg_vis.lpk
lazbuild --add-package $(pwd)/source/base/base_phone/pphones.lpk
lazbuild --add-package $(pwd)/source/base/base_options/poptions.lpk
lazbuild --add-package $(pwd)/source/components/richmemo/richmemopackage.lpk
lazbuild --add-package $(pwd)/source/base/pcmdprometapp.lpk
cd setup/i386-linux
echo "compiling apps..."
echo "compiling messagemanager..." > scompile-$2-apps.log
lazbuild $Params -B ../../source/messagemanager/messagemanager.lpi >> scompile-$2-apps.log
echo "compiling promet..." >> scompile-$2-apps.log
lazbuild $Params -q -B ../../source/promet.erp/prometerp.lpi >> scompile-$2-apps.log
echo "compiling statistics..." >> scompile-$2-apps.log
lazbuild $Params -q  ../../source/statistics/statistics.lpi  >> scompile-$2-apps.log
echo "compiling wizardmandant..." >> scompile-$2-apps.log
lazbuild $Params -q ../../source/tools/wizardmandant.lpi  >> scompile-$2-apps.log
echo "compiling import/exporters..."
echo "compiling sync_..." > scompile-$2-tools.log
lazbuild $Params -q -B ../../source/sync/sync_db.lpi  >> scompile-$2-tools.log
lazbuild $Params -q -B ../../source/sync/sync_owncloud.lpi  >> scompile-$2-tools.log
lazbuild $Params -q -B ../../source/sync/sync_redmine.lpi  >> scompile-$2-tools.log
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
echo "compiling pstarter..." >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/pstarter.lpi  >> scompile-$2-tools.log
echo "compiling cdmenue..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/cdmenue.lpi  >> scompile-$2-tools.log
echo "compiling cmdwizardmandant..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/cmdwizardmandant.lpi  >> scompile-$2-tools.log
echo "compiling checkin/out..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/checkin.lpi  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/checkout.lpi  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/tableedit.lpi  >> scompile-$2-tools.log
echo "compiling archivestore..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/archivestore.lpi  >> scompile-$2-tools.log
echo "compiling clientmanagement..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/clientmanagement.lpi  >> scompile-$2-tools.log
echo "compiling processmanager..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/processmanager.lpi  >> scompile-$2-tools.log
echo "compiling processdaemon..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/processdaemon.lpi  >> scompile-$2-tools.log
echo "compiling helpviewer..."  >> scompile-$2-tools.log
lazbuild $Params -q ../../source/tools/helpviewer.lpi  >> scompile-$2-tools.log
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
