#!/bin/bash

# Set processor architecture
if [ -z $CPU_TARGET ]; then
   export CPU_TARGET=$(fpc -iTP)
fi

export DC_APP_DIR=$1/prometerp.app
export DC_INSTALL_DIR=$DC_APP_DIR/Contents/MacOS

mkdir -p $DC_INSTALL_DIR

mkdir -p $DC_INSTALL_DIR/plugins

# directories
# Copy files
