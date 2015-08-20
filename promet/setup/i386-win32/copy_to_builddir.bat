set BASEVERSION=%1
set TARGETCPU=%2
md ..\executables\%BASEVERSION%
md ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\prometerp.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\pstarter.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\tools\processmanager.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\tools\messagemanager.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\helpviewer.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\tools\sync_*.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\tools\linksender.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\plugins\shipping_*.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\tools\*receiver.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\tools\*sender.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\tools\pscript.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\tools\pextracttext.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\statistics.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\meetingminutes.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\timeregistering.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\archivestore.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\wizardmandant.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\cmdwizardmandant.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\checkout.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\checkin.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\tableedit.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\clientmanagement.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\cdmenue.exe ..\executables\%BASEVERSION%\%TARGETCPU%
copy ..\..\output\i386-win32\plugins\*.wlx ..\executables\%BASEVERSION%\%TARGETCPU%
