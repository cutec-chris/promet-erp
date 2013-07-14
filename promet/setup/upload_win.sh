#!/bin/bash
Version=$(sed 's/\r//g' ../source/base/version.inc).$(sed 's/\r//g' ../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
scp output/*_$(echo $Version)_$1*.exe autoupload@ullihome.de:promet_upload_target
scp output/db_setup*.exe autoupload@ullihome.de:promet_upload_target
