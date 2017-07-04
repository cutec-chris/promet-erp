#!/bin/bash
. ./promet/setup/build-tools/setup_enviroment.sh
echo "==Alpha Builds==" > /tmp/act_alphadownload.txt
echo "Diese Builds wurden am $(date) von Version $BUILD_VERSION erstellt." >> /tmp/act_alphadownload.txt
echo "Diese Builds wurden vom Branch $CI_BUILD_REF_NAME erstellt." >> /tmp/act_alphadownload.txt
echo "===Windows===" >> /tmp/act_alphadownload.txt
. ./promet/setup/i386-win32/upload.sh i386 win32
echo "===Linux x86_64 (64 bit)===" >> /tmp/act_alphadownload.txt
. ./promet/setup/i386-linux/upload.sh x86_64 linux
echo "===Linux i386 (32 bit)===" >> /tmp/act_alphadownload.txt
. ./promet/setup/i386-linux/upload.sh i386 linux
echo "===Linux arm (z.b. Raspberry pi)===" >> /tmp/act_alphadownload.txt
. ./promet/setup/i386-linux/upload.sh arm linux
cat /tmp/act_alphadownload.txt