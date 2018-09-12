pipeline {
    agent any
    stages {
        stage('Checkout') {
            steps {
                checkout scm
            } 
        }
        stage('Checkout submodules') {
            agent {
                docker {
                    image 'cutec/buildhost-lazarus-x64'
                    args '-v $WORKSPACE:/root'
                }
            }            
            steps {
                unstash 'scm'
                sh '/root/build.sh submodules'
            }
        }
    }
}
