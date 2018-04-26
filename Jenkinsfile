pipeline {

    agent { label 'master' }

    triggers{
        pollSCM('* * * * *')
    }

    options {
        disableConcurrentBuilds()
        lock resource: 'dashboard-build'
    }

    environment{
        deployment_package_feed = credentials('Deployment_Package_Feed_Raw')
    }

    stages {
        stage('Build'){
            steps {
                bat 'yarn'
                bat 'yarn build'
            }
        }
        stage('Test'){
            steps {
                bat 'yarn test-output'
            }

            post {
                always{
                    junit 'junit.xml'
                }
            }
        }
        stage('Deploy'){
            when {
                branch 'master'
            }
            steps {
                powershell '''
                    Add-Type -Assembly "System.IO.Compression.FileSystem";
                    [System.IO.Compression.ZipFile]::CreateFromDirectory("dist\\docs", "Dashboard.Docs.0.1.$env:BUILD_NUMBER.zip") ;
                    $wc = new-object System.Net.WebClient
                    $wc.UploadFile("$env:DEPLOYMENT_PACKAGE_FEED", "Dashboard.Docs.0.1.$env:BUILD_NUMBER.zip")
                '''
                powershell '''
                    Add-Type -Assembly "System.IO.Compression.FileSystem";
                    [System.IO.Compression.ZipFile]::CreateFromDirectory("dist", "Dashboard.0.1.$env:BUILD_NUMBER.zip") ;
                    $wc = new-object System.Net.WebClient
                    $wc.UploadFile("$env:DEPLOYMENT_PACKAGE_FEED", "Dashboard.0.1.$env:BUILD_NUMBER.zip")
                '''          
            }
        }
        
        stage ('Deploy Pre-release'){
            when {
                not {
                    branch 'master'
                }
            }

            steps {
                powershell '''
                    Add-Type -Assembly "System.IO.Compression.FileSystem";
                    [System.IO.Compression.ZipFile]::CreateFromDirectory("dist\\docs", "Dashboard.Docs.0.1.$env:BUILD_NUMBER-$env:BRANCH_NAME.zip") ;
                    $wc = new-object System.Net.WebClient
                    $wc.UploadFile("$env:DEPLOYMENT_PACKAGE_FEED", "Dashboard.Docs.0.1.$env:BUILD_NUMBER-$env:BRANCH_NAME.zip")
                '''
                powershell '''
                    Add-Type -Assembly "System.IO.Compression.FileSystem";
                    [System.IO.Compression.ZipFile]::CreateFromDirectory("dist", "Dashboard.0.1.$env:BUILD_NUMBER-$env:BRANCH_NAME.zip") ;
                    $wc = new-object System.Net.WebClient
                    $wc.UploadFile("$env:DEPLOYMENT_PACKAGE_FEED", "Dashboard.0.1.$env:BUILD_NUMBER-$env:BRANCH_NAME.zip")
                '''
            }

        }
    }

    post {

        always {
            step ([$class: 'WsCleanup'])
        }
        success {
            slackSend color:"good", message: "Build Completed - ${env.JOB_NAME} ${env.BUILD_NUMBER} (<${env.BUILD_URL}|Open>)"
        }

        unstable {
            slackSend color:"warning", message:  "Build Partially Completed - ${env.JOB_NAME} ${env.BUILD_NUMBER} (<${env.BUILD_URL}|Open>)"
        }

        failure {
            slackSend color:"danger", message:  "Build Failed - ${env.JOB_NAME} ${env.BUILD_NUMBER} (<${env.BUILD_URL}|Open>)"
        }
    }

}
