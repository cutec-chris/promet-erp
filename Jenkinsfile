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
                label "Buildhost x64"
                docker {
                    image 'cutec/buildhost-lazarus-x64'
                    args '-v $WORKSPACE:/root'
                }
                customWorkspace '../workspace'
                }    
            }
            steps {
                sh '/root/build.sh submodules'
            }
        }
    }
}
