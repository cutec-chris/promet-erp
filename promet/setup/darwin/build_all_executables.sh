#!/bin/bash
cd darwin
echo "compiling for $1..."

echo "compiling apps..."
echo "compiling messagemanager..." >> scompile-$2.log
lazbuild -q -B ../../source/messagemanager/messagemanager.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/messagemanager
echo "compiling promet..." >> scompile-$2.log
lazbuild -q -B ../../source/promet.erp/prometerp.lpi > scompile-$2.log
strip ../../output/$2-darwin/prometerp
echo "compiling statistics..." >> scompile-$2.log
lazbuild -q ../../source/statistics/statistics.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/statistics
echo "compiling wizardmandant..." >> scompile-$2.log
lazbuild -q ../../source/tools/wizardmandant.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/wizardmandant
echo "compiling import/exporters..."
echo "compiling sync_db..." >> scompile-$2.log
lazbuild -q ../../source/sync/sync_db.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/sync_db
echo "compiling pop3receiver..." >> scompile-$2.log
lazbuild -q ../../source/sync/pop3receiver.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/pop3receiver
echo "compiling feedreceiver..." >> scompile-$2.log
lazbuild -q ../../source/sync/feedreceiver.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/feedreceiver
echo "compiling twitterreceiver..." >> scompile-$2.log
lazbuild -q ../../source/sync/twitterreceiver.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/twitterreceiver
echo "compiling smtpsender..." >> scompile-$2.log
lazbuild -q ../../source/sync/smtpsender.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/smtpsender
echo "compiling tools..."
echo "compiling pstarter..." >> scompile-$2.log
lazbuild -q ../../source/tools/pstarter.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/pstarter
echo "compiling cdmenue..."  >> scompile-$2.log
lazbuild -q ../../source/tools/cdmenue.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/cdmenue
echo "compiling cmdwizardmandant..."  >> scompile-$2.log
lazbuild -q ../../source/tools/cmdwizardmandant.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/cmdwizardmandant
echo "compiling checkin/out..."  >> scompile-$2.log
lazbuild -q ../../source/tools/checkin.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/checkin
lazbuild -q ../../source/tools/checkout.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/checkout
lazbuild -q ../../source/tools/tableedit.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/tableedit
echo "compiling archivestore..."  >> scompile-$2.log
lazbuild -q ../../source/tools/archivestore.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/archivestore
echo "compiling clientmanagement..."  >> scompile-$2.log
lazbuild -q ../../source/tools/clientmanagement.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/clientmanagement
echo "compiling helpviewer..."  >> scompile-$2.log
lazbuild -q ../../source/tools/helpviewer.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/helpviewer

echo "compiling webservices..."
echo "compiling webserver..."  >> scompile-$2.log
lazbuild -q ../../source/webservers/webserver.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/web/webserver
echo "compiling imapserver..."  >> scompile-$2.log
lazbuild -q ../../source/webservers/imapserver.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/web/imapserver
echo "compiling mta..."  >> scompile-$2.log
lazbuild -q ../../source/webservers/mta.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/web/mta
echo "compiling nntpserver..."  >> scompile-$2.log
lazbuild -q ../../source/webservers/nntpserver.lpi  >> scompile-$2.log
strip ../../output/$2-darwin/tools/nntpserver
