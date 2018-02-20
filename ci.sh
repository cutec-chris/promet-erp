#update submodules
docker run --rm -v /home/chris/gogs/jenkins/workspace/promet/Promet-ERP:/root cutec/buildhost-lazarus-x64 bash /root/build.sh submodules
#i386-windows
docker run --rm -v /home/chris/gogs/jenkins/workspace/promet/Promet-ERP:/root/promet cutec/buildhost-lazarus-windows bash z:/root/promet/build.sh all
#x86-64-linux
docker run --rm -v /home/chris/gogs/jenkins/workspace/Promet-ERP:/root cutec/buildhost-lazarus-x64 bash /root/build.sh
#i386-linux
docker run --rm -v /home/chris/gogs/jenkins/workspace/Promet-ERP:/root cutec/buildhost-lazarus-i386 bash /root/build.sh
#armhf-linux
#sudo apt-get install qemu-user-static
docker run --rm -v /home/chris/gogs/jenkins/workspace/Promet-ERP:/root -v /usr/bin/qemu-arm-static:/usr/bin/qemu-arm-static  cutec/buildhost-lazarus-armhf bash /root/build.sh server
#upload
cd /home/chris/gogs/jenkins/workspace/promet/Promet-ERP
bash promet/setup/upload_builds.sh
#change wiki
