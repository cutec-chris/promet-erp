pipeline {
    agent any
    stages {
        stage('Checkout') {
            steps {
                checkout scm
                sh "docker run --rm -v /docker/gogs/jenkins/home'${env.WORKSPACE.substring(17,env.WORKSPACE.length())}':'/root' cutec/buildhost-lazarus-x64 bash /root/build.sh submodules"
            } 
        }
        stage ("Build") {
            steps {
                parallel (
                    "Windows" : {
                        echo 'done'
                    },
                    "Linux" : {
                        echo 'done'
                    }
                )
     }/*            
          parallel (
            sh "docker run --rm -v /docker/gogs/jenkins/home'${env.WORKSPACE.substring(17,env.WORKSPACE.length())}':'/root/promet' cutec/buildhost-lazarus-windows bash z:/root/promet/build.sh all"
            sh "docker run --rm -v /docker/gogs/jenkins/home'${env.WORKSPACE.substring(17,env.WORKSPACE.length())}':'/root' cutec/buildhost-lazarus-x64 bash /root/build.sh"
            sh "docker run --rm -v /docker/gogs/jenkins/home'${env.WORKSPACE.substring(17,env.WORKSPACE.length())}':'/root' cutec/buildhost-lazarus-i386 bash /root/build.sh"
            sh "docker run --rm -v /docker/gogs/jenkins/home'${env.WORKSPACE.substring(17,env.WORKSPACE.length())}':'/root' -v /usr/bin/qemu-arm-static:/usr/bin/qemu-arm-static  cutec/buildhost-lazarus-armhf bash /root/build.sh server"
          )
*/          
    }
}
