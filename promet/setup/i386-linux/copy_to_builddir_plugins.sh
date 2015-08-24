#!/bin/bash
mkdir $2/plugins
cp ../../output/$1-linux/plugins/*.wlx $2/plugins
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
cp $2/plugins/*.wlx ../executables/$Version/$1
