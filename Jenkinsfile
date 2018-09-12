pipeline {
    agent any
    environment {
        WORKSPACE_HOST = WORKSPACE.
        DISABLE_AUTH = 'true'
        DB_ENGINE    = 'sqlite'
    }
    stages {
        stage('Checkout') {
            steps {
                checkout scm
                sh "docker run --rm -v /docker/gogs/jenkins/'${env.WORKSPACE.substring(17,100)}':'/root' cutec/buildhost-lazarus-x64 bash /root/build.sh submodules"
            } 
        }
    }
}
