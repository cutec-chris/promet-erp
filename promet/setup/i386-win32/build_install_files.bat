md ..\..\output
md ..\..\output\%1
md ..\..\output\%1\plugins
lazbuild -q ..\..\source\tools\wizardmandant.lpi
If errorlevel 1 lazbuild -q ..\..\source\tools\wizardmandant.lpi
If errorlevel 1 goto end
lazbuild -q -B ..\..\source\messagemanager\messagemanager.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\messagemanager\messagemanager.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\promet.erp\prometerp.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\promet.erp\prometerp.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\statistics\statistics.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\statistics\statistics.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\meeting\meeting.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\meeting\meeting.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\tools\cmdwizardmandant.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\tools\cmdwizardmandant.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\tools\pstarter.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\tools\pstarter.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\tools\linksender.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\tools\linksender.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\tools\checkout.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\tools\checkin.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\tools\tableedit.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\tools\tableedit.lpi
If errorlevel 1 goto end

lazbuild -q ..\..\source\timeregistering\timeregistering.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\timeregistering\timeregistering.lpi
If errorlevel 1 goto end

lazbuild -q ..\..\source\sync\sync_db.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\sync\sync_outlook_contacts.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\sync\sync_outlook_calendar.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\sync\sync_outlook_tasks.lpi
If errorlevel 1 goto end

lazbuild -q ..\..\source\tools\cdmenue.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\tools\cdmenue.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\tools\helpviewer.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\tools\helpviewer.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\tools\archivestore.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\tools\archivestore.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\tools\clientmanagement.lpi
If errorlevel 1 lazbuild -q -B ..\..\source\tools\clientmanagement.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\messageimport\pop3receiver.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\messageimport\smtpsender.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\messageimport\feedreceiver.lpi
If errorlevel 1 goto end
lazbuild -q ..\..\source\messageimport\twitterreceiver.lpi
If errorlevel 1 goto end
goto realend
:end
echo Compile done (or failed)...
pause
:realend
