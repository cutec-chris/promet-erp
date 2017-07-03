#!/bin/bash
basedir=$(pwd)
cd promet/source/components
. ../../setup/build-tools/setup_enviroment.sh
echo "Building components..."
# Build components
$lazbuild dexif/dexif_package.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed dexif"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild zvdatetimectrls/zvdatetimectrls.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed zvdatetimectrls"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild -b zeos/packages/lazarus/zcomponent.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed zeos"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild -b zeos/packages/lazarus/zcomponent_nogui.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed zeos_nogui"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild -b kcontrols/packages/kcontrols/kcontrolslaz.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed kcontrols"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild synapse/laz_synapse.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed synapse"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild websockets/websockets.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt

$lazbuild uxmpp/source/uxmpp_laz.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed xmpp"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild tvplanit/packages/v103_lazarus.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed turbopower vplanit"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild tmqttclient/TMQTTClient/laz_mqtt.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed mqtt"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild -b thumbs/thumbctrl.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed thumbctrl"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild tapi/laz_tapi.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed tapi"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild scanning/sanetools.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed sane"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild powerpdf/pack_powerpdf.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed powerpdf"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild pascalscript/Source/ppascalscriptfcl.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed pascalscript fcl"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild pascalscript/Source/ppascalscriptlcl.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed pascalscript lcl"
  tail -n 10 build.txt
fi
$lazbuild lnet/lazaruspackage/lnetbase.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
if [ $? -ne 0 ]; then
  echo "build failed lnet"
  tail -n 10 build.txt
  exit 1
fi
$lazbuild lazreport/lazreport_addons.lpk $BUILD_ARCH $BUILD_PARAMS > build.txt
cd $basedir
