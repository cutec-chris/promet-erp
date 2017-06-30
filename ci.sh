#!/bin/bash
docker run -it --name=ci_build_64 buildhost-lazarus-x86_64
docker exec ci_build_64 git clone --depth=1 http://192.168.177.120:10090/promet/promet-erp.git
docker exec ci_build_64 cd promet-erp
docker exec ci_build_64 git submodule sync --recursive
docker exec ci_build_64 git submodule update --init --recursive