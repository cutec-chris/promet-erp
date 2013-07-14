#!/bin/bash
Program=$2
Widgetset=$1
Version=$3
Arch=$4
Archfpc=$5
Date=$6
BuildDir=$7
cp ../output/*$Version_$Arch*.deb /tmp
cd /tmp
alien -r *.deb
