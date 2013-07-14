SET PROGNAME=Promet-ERP
SET PATH=%PATH%;C:\lazarus\fpc\2.6.1\bin\i386-win32
FOR /F %%L IN () DO SET TARGETOS=%%L
FOR /F %%L IN ('fpc -iTP') DO SET TARGETCPU=%%L
SET FULLTARGET=%TARGETCPU%-%TARGETOS%
FOR /F "delims='" %%F IN (..\..\source\base\version.inc) DO set BASEVERSION=%%F
FOR /F "delims='" %%F IN (..\..\source\base\revision.inc) DO set BASEREVISION=%%F
set BASEREVISION=%BASEREVISION: =%
set BASEVERSION=%BASEVERSION%.%BASEREVISION%
md ..\executables\%BASEVERSION%
md ..\executables\%BASEVERSION%\%TARGETCPU%
REM call clean_all.bat
REM call build_install_files.bat FULLTARGET
REM call copy_to_builddir.bat BASEVERSION TARGETCPU
call build_setup.bat TARGETOS TARGETCPU BASEVERSION
call build_tr_setup.bat TARGETOS TARGETCPU BASEVERSION
