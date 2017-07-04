#!/bin/bash
DIR=$(dirname "$1")
FILE=$(basename "$1")

AUTOUPLOAD_PORT=10202
AUTOUPLOAD_USER=root
AUTOUPLOAD_HOST=downloads.free-erp.de

echo "uploading $1..."
scp -P $AUTOUPLOAD_PORT $basedir/promet/setup/output/$1 $AUTOUPLOAD_USER@$AUTOUPLOAD_HOST:/sites/
if [ "x$?" = "x0" ]
  then
  echo "[http://downloads.free-erp.de/$1 $1]" >> /tmp/act_alphadownload.txt
  echo "Upload erfolgreich, Alpha Eintrag !"
fi
#scp $2 christian_u@frs.sourceforge.net:/home/frs/project/Promet-ERP/
