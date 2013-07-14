SET PROGNAME=Zeiterfassung Promet-ERP
SET TARGETOS=%1
SET TARGETCPU=%2
SET FULLTARGET=%TARGETCPU%-%TARGETOS%
SET BASEVERSION=%3
set VERSION=%BASEVERSION%
iscc Timeregistering.iss
If errorlevel 1 goto end
goto realend
:end
echo Compile done (or failed)...
pause
:realend
