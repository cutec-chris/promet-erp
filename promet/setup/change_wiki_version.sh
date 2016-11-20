#!/bin/bash
basedir=$(pwd)
if [ "x$STORA_CONN" != "x" ]; then
  mkdir ~/.prometerp
  echo "SQL" > ~/.prometerp/Stora.perml
  echo $STORA_CONN >> ~/.prometerp/Stora.perml
fi
. ./promet/setup/i386-linux/change_wiki_linux.sh i386 i386
. ./promet/setup/i386-linux/change_wiki_linux.sh x86_64 amd64
. ./promet/setup/i386-linux/change_wiki_linux.sh arm arm
. ./promet/setup/i386-win32/change_wiki_windows.sh
. ./promet/setup/portableapps/change_wiki.sh

cd $basedir/promet/setup/output
for f in *$TARGET_CPU-$TARGET_OS-$BUILD_VERSION.zip
do
  echo "Processing $f file..."
  # take action on each file. $f store current file name
  targetfile=$f
  targetcur=$target-current.zip
  ssh $AUTOUPLOAD_USER@$AUTOUPLOAD_HOST -p $AUTOUPLOAD_PORT "cd promet_upload_target;ln -s -f $targetfile $targetcur"
  done
cd $basedir


cd $basedir