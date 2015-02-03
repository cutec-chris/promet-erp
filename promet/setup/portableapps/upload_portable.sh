#!/bin/bash
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
chmod 644 output/*
scp ../output/*.paf.exe autoupload@178.254.12.54:promet_upload_target
scp ../output/*portable.zip autoupload@178.254.12.54:promet_upload_target
scp ../output/*portable-firebird.zip autoupload@178.254.12.54:promet_upload_target
