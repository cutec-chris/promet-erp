#!/bin/bash
lazbuild ../../source/tools/changewikipage.lpi
Version=$(sed 's/\r//g' ../../source/base/version.inc).$(sed 's/\r//g' ../../source/base/revision.inc)
Version=$(echo $Version | sed 's/\n//g');
Arch=`dpkg --print-architecture`
Archfpc=$(fpc -h | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
Date=`date`
Rpm64Size=$(ls -s --block-size=1048576 "../output/promet-erp-$(echo $Version)-2.x86_64.rpm" | cut -d' ' -f1)
Rpm32Size=$(ls -s --block-size=1048576 "../output/promet-erp-$(echo $Version)-2.i386.rpm" | cut -d' ' -f1)
Deb64Size=$(ls -s --block-size=1048576 "../output/promet-erp_$(echo $Version)_amd64-gtk2.deb" | cut -d' ' -f1)
Deb32Size=$(ls -s --block-size=1048576 "../output/promet-erp_$(echo $Version)_i386-gtk2.deb" | cut -d' ' -f1)

TimeRpm64Size=$(ls -s --block-size=1048576 "../output/promet-erp-timeregistering-$(echo $Version)-2.x86_64.rpm" | cut -d' ' -f1)
TimeRpm32Size=$(ls -s --block-size=1048576 "../output/promet-erp-timeregistering-$(echo $Version)-2.i386.rpm" | cut -d' ' -f1)
TimeDeb64Size=$(ls -s --block-size=1048576 "../output/promet-erp-timeregistering_$(echo $Version)_amd64-gtk2.deb" | cut -d' ' -f1)
TimeDeb32Size=$(ls -s --block-size=1048576 "../output/promet-erp-timeregistering_$(echo $Version)_i386-gtk2.deb" | cut -d' ' -f1)

StatisticsRpm64Size=$(ls -s --block-size=1048576 "../output/promet-erp-statistics-$(echo $Version)-2.x86_64.rpm" | cut -d' ' -f1)
StatisticsRpm32Size=$(ls -s --block-size=1048576 "../output/promet-erp-statistics-$(echo $Version)-2.i386.rpm" | cut -d' ' -f1)
StatisticsDeb64Size=$(ls -s --block-size=1048576 "../output/promet-erp-statistics_$(echo $Version)_amd64-gtk2.deb" | cut -d' ' -f1)
StatisticsDeb32Size=$(ls -s --block-size=1048576 "../output/promet-erp-statistics_$(echo $Version)_i386-gtk2.deb" | cut -d' ' -f1)

ToolsRpm64Size=$(ls -s --block-size=1048576 "../output/promet-erp-tools-$(echo $Version)-2.x86_64.rpm" | cut -d' ' -f1)
ToolsRpm32Size=$(ls -s --block-size=1048576 "../output/promet-erp-tools-$(echo $Version)-2.i386.rpm" | cut -d' ' -f1)
ToolsDeb64Size=$(ls -s --block-size=1048576 "../output/promet-erp-tools_$(echo $Version)_amd64-gtk2.deb" | cut -d' ' -f1)
ToolsDeb32Size=$(ls -s --block-size=1048576 "../output/promet-erp-tools_$(echo $Version)_i386-gtk2.deb" | cut -d' ' -f1)

cat downloads_linux64.txt | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/ARCHFPC/$Archfpc/g" \
      -e "s/WIN_SIZE/$WinSize Mb/g" \
      -e "s/WIN_TOOLS_SIZE/$WinToolsSize Mb/g" \
      -e "s/WIN_TR_SIZE/$WinTRSize Mb/g" \
      -e "s/WIN_PDB_SIZE/$WinPDBSize Mb/g" \
      -e "s/RPM64_SIZE/$Rpm64Size Mb/g" \
      -e "s/RPM32_SIZE/$Rpm32Size Mb/g" \
      -e "s/DEB64_SIZE/$Deb64Size Mb/g" \
      -e "s/DEB32_SIZE/$Deb32Size Mb/g" \
      -e "s/STATISTICSR64_SIZE/$StatisticsRpm64Size Mb/g" \
      -e "s/STATISTICSR32_SIZE/$StatisticsRpm32Size Mb/g" \
      -e "s/STATISTICSD64_SIZE/$StatisticsDeb64Size Mb/g" \
      -e "s/STATISTICSD32_SIZE/$StatisticsDeb32Size Mb/g" \
      -e "s/TIMEREGISTERINGR64_SIZE/$TimeRpm64Size Mb/g" \
      -e "s/TIMEREGISTERINGR32_SIZE/$TimeRpm32Size Mb/g" \
      -e "s/TIMEREGISTERINGD64_SIZE/$TimeDeb64Size Mb/g" \
      -e "s/TIMEREGISTERINGD32_SIZE/$TimeDeb32Size Mb/g" \
      -e "s/TOOLSR64_SIZE/$ToolsRpm64Size Mb/g" \
      -e "s/TOOLSR32_SIZE/$ToolsRpm32Size Mb/g" \
      -e "s/TOOLSD64_SIZE/$ToolsDeb64Size Mb/g" \
      -e "s/TOOLSD32_SIZE/$ToolsDeb32Size Mb/g" \
      -e "s/CREATEDDATE/$Date/g" \
  > act_downloads_linux64.txt
../../output/x86_64-linux/changewikipage --mandant=Stora Promet-ERP/dowloadplattforms/linux64 act_downloads_linux64.txt
../../output/x86_64-linux/changewikipage --mandant=Stora Promet-ERP/changes ../source/base/changes.txt
../../output/x86_64-linux/tools/sync_db --mandant=Stora