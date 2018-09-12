pipeline {
    agent any
    stages {
        stage('Checkout') {
            steps {
                checkout scm
                sh "docker run --rm -v '${env.WORKSPACE}':/root cutec/buildhost-lazarus-x64 bash /root/build.sh submodules"
            } 
        }
    }
}
