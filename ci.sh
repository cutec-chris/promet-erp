#!/bin/bash
docker run --name=ci_build_64 build-lazarus-x64
docker exec ci_build_64 git clone --depth=1 http://192.168.177.120:10090/promet/promet-erp.git
docker exec ci_build_64 cd promet-erp
docker exec ci_build_64 git submodule sync --recursive
docker exec ci_build_64 git submodule update --init --recursive

docker kill ci_build_64
docker rm ci_build_64