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
               sh "docker run --rm -v '${env.WORKSPACE}':'/project'  busybox cat /project/Dockerfile"
           }    
       }        
    }
}
