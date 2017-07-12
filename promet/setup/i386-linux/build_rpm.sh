#!/bin/bash
basedir=$(pwd)
cd promet/setup/i386-linux
. ../../setup/build-tools/setup_enviroment.sh
cd ../output
fakeroot alien -r *.deb
