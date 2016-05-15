#!/bin/bash
basedir=$(pwd)
cd promet/source/components
. ../../setup/build-tools/setup_enviroment.sh
echo "Building components..."
# Build components
$lazbuild dexif/dexif_package.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed dexif"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild zvdatetimectrls/zvdatetimectrls.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed zvdatetimectrls"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild -b zeos/packages/lazarus/zcomponent.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed zeos"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild -b zeos/packages/lazarus/zcomponent_nogui.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed zeos_nogui"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild -b kcontrols/packages/kcontrols/kcontrolslaz.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed kcontrols"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild synapse/laz_synapse.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed synapse"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild websockets/websockets.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed websockets"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild uxmpp/source/uxmpp_laz.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed xmpp"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild tvplanit/packages/v103_lazarus.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed turbopower vplanit"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild tmqttclient/TMQTTClient/laz_mqtt.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed mqtt"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild -b thumbs/thumbctrl.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed thumbctrl"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild tapi/laz_tapi.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed tapi"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild scanning/sanetools.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed sane"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild powerpdf/pack_powerpdf.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed powerpdf"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild pascalscript/Source/ppascalscriptfcl.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed pascalscript fcl"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild pascalscript/Source/ppascalscriptlcl.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed pascalscript lcl"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild lnet/lazaruspackage/lnetbase.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed lnet"
  $grep -w "Error:" build.txt
  exit 1
fi
$lazbuild lazreport/lazreport_addons.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ "$?" -ne "0" ]; then
  echo "build failed lazreport addons"
  $grep -w "Error:" build.txt
  exit 1
fi
cd $basedir
