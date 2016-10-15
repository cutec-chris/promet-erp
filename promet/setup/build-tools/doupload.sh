#!/bin/bash
DIR=$(dirname "$1")
FILE=$(basename "$1")

AUTOUPLOAD_PORT=232
AUTOUPLOAD_USER=autoupload
AUTOUPLOAD_HOST=downloads.free-erp.de

echo "uploading $1..."
scp -P $AUTOUPLOAD_PORT $basedir/promet/setup/output/$1 $AUTOUPLOAD_USER@$AUTOUPLOAD_HOST:promet_upload_target
if [ "x$?" = "x0" ]
  then
  ssh $AUTOUPLOAD_USER@$AUTOUPLOAD_HOST -p $AUTOUPLOAD_PORT "cd promet_upload_target;ln -s -f $FILE $2"
  echo "[downloads.free-erp.de/$1 $1]" >> act_alphadownload.txt
  echo "Upload erfolgreich, Alpha Eintrag !"
fi
#scp $2 christian_u@frs.sourceforge.net:/home/frs/project/Promet-ERP/
