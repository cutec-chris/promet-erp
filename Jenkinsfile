pipeline {
    agent any
    stages {
        stage('Checkout') {
            steps {
                checkout scm
                sh "docker run --rm -v /docker/gogs/jenkins/home'${env.WORKSPACE.substring(17,env.WORKSPACE.length())}':'/root' cutec/buildhost-lazarus-x64 bash /root/build.sh submodules"
            } 
        }
    }
}
