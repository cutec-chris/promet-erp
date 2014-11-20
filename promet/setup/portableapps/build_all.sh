#!/bin/bash
Program=Promet-ERP
Widgetset=$2
if [ "x$Widgetset" = "x" ]; then
  Widgetset=gtk2
fi
sh build_standard.sh
sh build_firebird.sh
