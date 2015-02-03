#!/bin/bash
Version=$(sed 's/\r//g' ../source/base/version.inc).$(sed 's/\r//g' ../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
cd portableapps
sh build_all.sh
sh upload_portable.sh
sh change_wiki.sh
cd ..