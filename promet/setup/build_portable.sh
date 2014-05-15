#!/bin/bash
Version=$(sed 's/\r//g' ../source/base/version.inc).$(sed 's/\r//g' ../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
cd portableapps
FULL_NAME=$(cd `dirname $0` && pwd)
WIN_DIR=$(echo $FULL_NAME | sed 's/\//\\/g')
sh build_all.sh
cd ..