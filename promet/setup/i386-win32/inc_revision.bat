FOR /F "delims='" %%F IN (..\..\source\base\revision.inc) DO set BASEREVISION=%%F
set BASEREVISION=%BASEREVISION: =%
SET /A BASEREVISION+=1
echo %BASEREVISION% > ..\..\source\base\revision.inc

