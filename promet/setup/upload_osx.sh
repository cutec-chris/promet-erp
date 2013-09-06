#!/bin/bash
Version=$(cat ../source/base/version.inc).$(sed 's/\r//g' ../source/base/revision.inc)
#Version=$(echo $Version | sed 's/\n//g');
echo $Version
chmod 644 output/*
scp output/*_$(echo $Version)_*.dmg autoupload@ullihome.de:promet_upload_target
