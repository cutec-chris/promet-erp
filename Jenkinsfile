pipeline {
    agent any
    stages {
        stage('Checkout') {
            steps {
                checkout scm
            } 
        }
       stage('Checkout submodules') {
            steps {
                unstash 'scm'
                script{
                    docker.image('cutec/buildhost-lazarus-x64').inside{ 
                        sh '/root/build.sh submodules'
                    }
                }
            }
        }        
    }
}
