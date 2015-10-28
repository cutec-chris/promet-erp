#!/bin/bash
basedir=$(pwd)
cd promet/source/components
. ../../setup/build-tools/setup_enviroment.sh
echo "Building components..."
# Build components
$lazbuild dexif/dexif_package.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 1"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild zvdatetimectrls/zvdatetimectrls.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 2"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild zeos/packages/lazarus/zcomponent.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 3"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild zeos/packages/lazarus/zcomponent_nogui.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 4"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild websockets/websockets.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 5"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild uxmpp/source/uxmpp_laz.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 6"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild tvplanit/packages/v103_lazarus.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 7"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild tmqttclient/TMQTTClient/laz_mqtt.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 8"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild thumbs/thumbctrl.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 9"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild tapi/laz_tapi.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 10"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild synapse/laz_synapse.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 11"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild scanning/sanetools.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 12"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild powerpdf/pack_powerpdf.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 13"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild pascalscript/Source/ppascalscriptfcl.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 15"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild pascalscript/Source/ppascalscriptlcl.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 14"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild lnet/lazaruspackage/lnetbase.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 16"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild lazreport/lazreport_addons.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed 17"
  $grep -w "Error:" build.txt
  exit 1
fi
cd $basedir
