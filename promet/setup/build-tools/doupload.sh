#!/bin/bash
DIR=$(dirname "$1")
FILE=$(basename "$1")
echo "uploading $1..."
scp -P 232 $basedir/promet/setup/output/$BUILD_VERSION/$1 autoupload@178.254.12.54:promet_upload_target
ssh autoupload@178.254.12.54 -p 232 "cd promet_upload_target;ln -s -f $FILE $2"
#scp $2 christian_u@frs.sourceforge.net:/home/frs/project/Promet-ERP/
