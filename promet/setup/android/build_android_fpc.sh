ANDROID_NDK=/media/Daten/android/android-ndk
export ANDROID_BIN=$ANDROID_NDK/toolchains/arm-linux-androideabi-4.4.3/prebuilt/linux-x86/arm-linux-androideabi/bin

if [ -d ./2.6_fixes ]; then
  svn update ./2.6_fixes
else
  svn co http://svn.freepascal.org/svn/fpc/branches/fixes_2_6 ./2.6_fixes
fi

cd 2.6_fixes

make distclean
#FPC=$PPCBIN
#make build OS_TARGET=linux CPU_TARGET=arm OPT="-dFPC_ARMEL" CROSSBINDIR=$ANDROID_BIN
#sudo make FPC=`pwd`/compiler/ppcrossarm PREFIX=/opt/fpc crossinstall OS_TARGET=linux CPU_TARGET=arm OPT="-dFPC_ARMEL" CROSSBINDIR=$ANDROID_BIN
sudo make crossinstall CPU_TARGET=arm OS_TARGET=linux CROSSBINDIR=/media/Daten/android/android-ndk/toolchains/arm-linux-androideabi-4.4.3/prebuilt/linux-x86/arm-linux-androideabi/bin OPT=-dFPC_ARMEL
cd ..
