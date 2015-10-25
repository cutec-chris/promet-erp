#!/bin/bash
scp -P 232 $basedir/promet/setup/output/$BUILD_VERSION/$1 autoupload@178.254.12.54:promet_upload_target
ssh autoupload@178.254.12.54 -p 232 ln promet_upload_target/$2 promet_upload_target/$1