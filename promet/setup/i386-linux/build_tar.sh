#!/bin/bash
Program=$2
Widgetset=$1
Version=$3
Arch=$4
Archfpc=$5
Date=$6
BuildDir=$7

echo "build tar.gz..."
rm ../output/${Program}_$Version_$Arch-$Widgetset.tar.gz
cd $BuildDir
rm ${Program}_$Version_$Arch-$Widgetset.tar.gz
tar -czf ${Program}_$Version_$Arch-$Widgetset.tar.gz ./usr
