pipeline {
    agent any
    stages {
        stage('Checkout') {
            steps {
                checkout scm
                sh "docker run --rm -v '$(echo ${env.WORKSPACE} | sed 's_/var/jenkins_home_/docker/gogs/jenkins/_g')':'/root' cutec/buildhost-lazarus-x64 bash /root/build.sh submodules"
            } 
        }
    }
}
