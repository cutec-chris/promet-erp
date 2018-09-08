pipeline {
    agent any
    stages {
        stage('Checkout') {
          checkout scm
          sh 'docker run --rm -v $WORKSPACE:/root cutec/buildhost-lazarus-x64 bash /root/build.sh submodules'
        }
        stage ("Build") {
          parallel (
             #i386-windows
            sh 'docker run --rm -v $WORKSPACE:/root/promet cutec/buildhost-lazarus-windows bash z:/root/promet/build.sh all'
            #x86-64-linux
            sh 'docker run --rm -v $WORKSPACE:/root cutec/buildhost-lazarus-x64 bash /root/build.sh'
            #i386-linux
            sh 'docker run --rm -v $WORKSPACE:/root cutec/buildhost-lazarus-i386 bash /root/build.sh'
            #armhf-linux
            #sudo mount binfmt_misc -t binfmt_misc /proc/sys/fs/binfmt_misc
            #sudo echo ':arm:M::\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x28\x00:\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff:/usr/bin/qemu-arm-static:' > /proc/sys/fs/binfmt_misc/register
            #sudo apt-get install qemu-user-static
            sh 'docker run --rm -v $WORKSPACE:/root -v /usr/bin/qemu-arm-static:/usr/bin/qemu-arm-static  cutec/buildhost-lazarus-armhf bash /root/build.sh server'
          )
        }
        stage ("Deploy") {
          parallel (
            #upload
            sh 'cd /home/chris/gogs/jenkins/workspace/promet/Promet-ERP'
            sh 'bash promet/setup/upload_builds.sh'
            #change wiki
          )
        }  
    }
}
