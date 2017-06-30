#x86-64
docker run --rm -v /home/chris/gogs/jenkins/workspace/Promet-ERP:/root build-lazarus-x64 bash /root/build.sh
#i386
docker run --rm -v /home/chris/gogs/jenkins/workspace/Promet-ERP:/root build-lazarus-i386 bash /root/build.sh
#armhf
#sudo apt-get install qemu-user-static
docker run --rm -v /home/chris/gogs/jenkins/workspace/Promet-ERP:/root -v /usr/bin/qemu-arm-static:/usr/bin/qemu-arm-static  build-lazarus-armhf bash /root/build.sh server