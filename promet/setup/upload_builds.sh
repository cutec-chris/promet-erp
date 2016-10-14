#!/bin/bash
. ./promet/setup/build-tools/setup_enviroment.sh
. ./promet/setup/i386-win32/upload.sh i386 win32
. ./promet/setup/i386-linux/upload.sh i386 linux
. ./promet/setup/i386-linux/upload.sh x86_64 linux
. ./promet/setup/i386-linux/upload.sh arm linux