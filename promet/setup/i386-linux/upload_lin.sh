#!/bin/bash
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
chmod 644 output/*
scp ../output/*_$(echo $Version)_$1-*.deb autoupload@ullihome.de:promet_upload_target
#scp ../output/*$(echo $Version)-2.$2.rpm autoupload@ullihome.de:promet_upload_target
