#!/bin/bash
FULL_NAME=$(cd `dirname $0` && pwd)
#wget ftp://newwiki.freepascal.org/pub/lazarus/snapshots/Lazarus-1.1-39266-fpc-2.6.1-20121110-win32.exe
#wineprefixcreate --prefix $FULL_NAME/../../lazarus_wine
#WINEPREFIX=$FULL_NAME/../../lazarus_wine/ wine "lazarus-1.4.0-fpc-2.6.4-win32.exe"
#cd $FULL_NAME/../../lazarus_wine/drive_c/lazarus
#WINEPREFIX=$FULL_NAME/../../lazarus_wine/ wineconsole make

#wget http://www.jrsoftware.org/download.php/ispack.exe
WINEPREFIX=$FULL_NAME/../../lazarus_wine/ wine ispack.exe

