pipeline {
    agent any
    stages {
        stage('Checkout') {
          checkout scm
        }
        stage('Checkout submodules') {
            agent {
                docker {
                    image 'cutec/buildhost-lazarus-x64'
                    args '-v $WORKSPACE:/root'
                }
            }
            steps {
                sh '/root/build.sh submodules'
            }
        }
    }
}
