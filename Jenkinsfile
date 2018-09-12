pipeline {
    agent any
    stages {
        stage('Checkout') {
            steps {
                checkout scm
                sh "docker run --rm -v /docker/gogs/jenkins/'${env.WORKSPACE.substring(17,100)}':'/root' cutec/buildhost-lazarus-x64 bash /root/build.sh submodules"
            } 
        }
    }
}
