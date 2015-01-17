#!/bin/bash
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
chmod 644 output/*
scp -p 232 ../output/*_$(echo $Version)_$1-*.deb autoupload@178.254.12.54:promet_upload_target
scp -p 232 ../output/*$(echo $Version)-2.$2.rpm autoupload@178.254.12.54:promet_upload_target &
