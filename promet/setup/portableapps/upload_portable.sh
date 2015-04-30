#!/bin/bash
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
chmod 644 output/*
scp -P 232 ../output/*.paf.exe autoupload@178.254.12.54:promet_upload_target
scp -P 232 ../output/*portable.zip autoupload@178.254.12.54:promet_upload_target
scp -P 232 ../output/*portable-firebird.zip autoupload@178.254.12.54:promet_upload_target
