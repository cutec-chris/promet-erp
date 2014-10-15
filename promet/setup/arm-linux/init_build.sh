#!/bin/bash
Params='--cpu='$2' --build-mode=Default'
#if [ "x$2" = "xi386" ]; then
#  Params=$Params' --compiler=/usr/local/lib/fpc/2.7.1/ppc386'
#fi
echo "compiling for $1... $Params"
cd ../../
lazbuild --add-package $(pwd)/source/base/base_help/phelp.lpk
lazbuild --add-package $(pwd)/source/base/base_frames/pvisualframes.lpk
lazbuild --add-package $(pwd)/source/base/base_docmanage/pdocmanage.lpk
lazbuild --add-package $(pwd)/source/base/base_forms/pvisualforms.lpk
lazbuild --add-package $(pwd)/source/base/base_wiki/rtfconvert_pkg_vis.lpk
lazbuild --add-package $(pwd)/source/base/base_phone/pphones.lpk
lazbuild --add-package $(pwd)/source/base/base_options/poptions.lpk
lazbuild --add-package $(pwd)/source/components/richmemo/richmemopackage.lpk
lazbuild --add-package $(pwd)/source/base/pcmdprometapp.lpk
