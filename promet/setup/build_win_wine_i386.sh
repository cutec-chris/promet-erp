#!/bin/bash
Version=$(sed 's/\r//g' ../source/base/version.inc).$(sed 's/\r//g' ../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
cd i386-win32
FULL_NAME=$(cd `dirname $0` && pwd)
WIN_DIR=$(echo $FULL_NAME | sed 's/\//\\/g')
WIN_DIR='Z:\'$WIN_DIR
WINEPREFIX=$FULL_NAME/../../../lazarus_wine/ wineconsole "$WIN_DIR\clean_all.bat" i386-win32
WINEPREFIX=$FULL_NAME/../../../lazarus_wine/ wineconsole "$WIN_DIR\build_install_files.bat" i386-win32
WINEPREFIX=$FULL_NAME/../../../lazarus_wine/ wineconsole "$WIN_DIR\copy_to_builddir.bat" $Version i386
WINEPREFIX=$FULL_NAME/../../../lazarus_wine/ wineconsole "$WIN_DIR\build_setup.bat" win32 i386 $Version
WINEPREFIX=$FULL_NAME/../../../lazarus_wine/ wineconsole "$WIN_DIR\build_tr_setup.bat" win32 i386 $Version
WINEPREFIX=$FULL_NAME/../../../lazarus_wine/ wineconsole "$WIN_DIR\build_db_setup.bat" win32 i386 $Version
chmod 644 ../output/*.exe
sh upload_win.sh i386
sh change_wiki_windows.sh
cd ..