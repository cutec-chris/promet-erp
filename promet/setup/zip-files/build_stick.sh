#!/bin/bash
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
Program=Promet-ERP
BuildDir=../output/prometerp_$Version
echo "Creating Dirs"
mkdir $BuildDir
mkdir $BuildDir/prometerp
mkdir $BuildDir/prometerp/languages
mkdir $BuildDir/prometerp/resources
./copy_files_windows.sh $BuildDir/prometerp i386 $Version
mkdir $BuildDir/prometerp64
mkdir $BuildDir/prometerp64/languages
mkdir $BuildDir/prometerp64/resources
echo "Creating zip"
rm ../output/prometerp_$(echo $Version)_win32_i386.zip
cd $BuildDir/prometerp
zip -rq ../../prometerp_$(echo $Version)_win32_i386.zip .
cd ../../../zip-files
echo "Creating Stick version"
./copy_files_linux.sh $BuildDir/prometerp i386 $Version
./copy_files_linux.sh $BuildDir/prometerp64 x86_64 $Version
cp zip/cdmenue.*.html $BuildDir
cp stick/autorun.inf $BuildDir
cp stick/autorun $BuildDir
cp ../executables/$Version/i386/cdmenue $BuildDir/start_linux
cp ../executables/$Version/x86_64/cdmenue $BuildDir/start_linux_x86_64
cp ../executables/$Version/i386/cdmenue.exe $BuildDir/start_windows.exe
mkdir $BuildDir/prometerp/configurations
cp stick/mandant.dbf $BuildDir/prometerp/configurations
cp stick/mandant.dbt $BuildDir/prometerp/configurations
cp stick/prometerp.db $BuildDir
cd $BuildDir
zip -rq ../../output/prometerp_$(echo $Version)_stick.zip .
cd ../../zip-files
echo "Creating CD version"
rm ../output/prometerp_cd_$Version.iso
mkdir $BuildDir/installations
Filename = $Version
cp ../output/prometerp_$(echo $Version)_i386.exe $BuildDir/installations/promet-erp_i386-win32.exe
cp ../output/promet-erp_$(echo $Version)_i386-gtk2.deb $BuildDir/installations/promet-erp_i386-gtk2.deb
cp ../output/promet-erp_$(echo $Version)_amd64-gtk2.deb $BuildDir/installations/promet-erp_amd64-gtk2.deb
cp ../output/promet-erp-$(echo $Version)-2.i386.rpm $BuildDir/installations/promet-erp_i386-gtk2.rpm
cp ../output/promet-erp-$(echo $Version)-2.x86_64.rpm $BuildDir/installations/promet-erp_amd64-gtk2.rpm
rm $BuildDir/prometerp/cdmenue.*.html
cp cd/cdmenue.*.html $BuildDir/prometerp
mkisofs -R -J -o ../output/prometerp_cd_$Version.iso $BuildDir

rm -r $BuildDir

