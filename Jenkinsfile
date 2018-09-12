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
                node {
                    label 'Buildhost x64'
                    customWorkspace '../workspace'
                }
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
