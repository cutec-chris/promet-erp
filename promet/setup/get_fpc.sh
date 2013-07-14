#steps to build fpc and cross compilers
#get bootstrap compiler from fpc stable ftp (single executable)
#build trunk or fixes with bootstrap compiler
#build crosscompilers with bootstrap compiler

#!/bin/bash
#SYSTEM_FPC_COMPILER="/usr/local/lib/fpc/2.7.1/ppcx64"

#get bootstrap compiler to build final compiler
SYSTEM_FPC_COMPILER=/media/Daten/fpc/2.6_fixes/x86_64-linux-ppcx64
#wget ftp://freepascal.stack.nl/pub/fpc/dist/2.6.0/bootstrap/x86_64-linux-ppcx64.bz2
#svn co http://svn.freepascal.org/svn/fpc/branches/fixes_2_6 fpc/2.6_fixes
#mkdir fpc/2.6.1
#cd fpc/2.6.1
#svn co http://svn.freepascal.org/svn/fpc/2_6_1 fpc/2.6.1
#svn co http://svn.freepascal.org/svn/fpc/trunk fpc/trunk
cd fpc/trunk

#sudo apt-get install ia32-libs,wine
cd utils/fpcm
make
cd ../../
utils/fpcm/fpcmake -Tall
#make clean PP=${SYSTEM_FPC_COMPILER}
make build PP=${SYSTEM_FPC_COMPILER}
su -c "make install"
make build CPU_TARGET=i386 PP=${SYSTEM_FPC_COMPILER}
su -c "make install CPU_TARGET=i386 FPC=${SYSTEM_FPC_COMPILER}"
make build OS_TARGET=win32 CPU_TARGET=i386 PP=${SYSTEM_FPC_COMPILER}
su -c "make install OS_TARGET=win32 CPU_TARGET=i386 FPC=${SYSTEM_FPC_COMPILER}"
make build OS_TARGET=win64 PP=${SYSTEM_FPC_COMPILER}
su -c "make install OS_TARGET=win64 CPU_TARGET=x86_64 FPC=${SYSTEM_FPC_COMPILER}"
#make build CROSSOPT="-O2 -g -FD/media/Daten/fpc/java/" CPU_TARGET=jvm OS_TARGET=android ALLOW_WARNINGS=1 PP=${SYSTEM_FPC_COMPILER}
#linking fails (maybe jasmin isnt there)
#su -c "make install OS_TARGET=android CPU_TARGET=jvm FPC=${SYSTEM_FPC_COMPILER}"
#not functional
#sudo make build CPU_TARGET=avr OS_TARGET=embedded PP=${SYSTEM_FPC_COMPILER}
cd ../..

#cd /usr/local/lib/fpc/2.7.1
#sudo ./samplecfg /usr/local/lib/fpc/2.7.1
