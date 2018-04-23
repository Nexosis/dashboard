pipeline {

    agent { label 'master' }

    triggers{
        gitlab(triggerOnPush: true, triggerOnMergeRequest:true, branchFilterType: 'All')
    }

    options {
        gitLabConnection('GitLab')
        gitlabBuilds(builds: ['Build', 'Test', 'Deploy'])
        disableConcurrentBuilds()
    }

    environment{
        deployment_package_feed = credentials('Deployment_Package_Feed_Raw')
    }

    stages {
        stage('Build'){
            steps {
                updateGitlabCommitStatus name: 'Build', state: 'running'
                bat 'yarn'
                bat 'yarn build'
                updateGitlabCommitStatus name: 'Build', state: 'success'
            }

            post {
                failure {
                    updateGitlabCommitStatus name: 'Build', state: 'failed'
                }
            }
        }
        stage('Test'){
            steps {
                updateGitlabCommitStatus name: 'Test', state: 'running'
                bat 'yarn test-output'
                updateGitlabCommitStatus name: 'Test', state: 'success'
            }

            post {
                always{
                    junit 'junit.xml'
                }

                failure {
                    updateGitlabCommitStatus name: 'Test', state: 'failed'
                }
            }
        }
        stage('Deploy'){
            when {
                branch 'master'
            }
            steps {
                updateGitlabCommitStatus name: 'Deploy', state: 'running'
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
                updateGitlabCommitStatus name: 'Deploy', state: 'success'
            }

            post {
                failure {
                    updateGitlabCommitStatus name: 'Deploy', state: 'failed'
                }
            }
        }
        
        stage ('Deploy Pre-release'){
            when {
                not {
                    branch 'master'
                }
            }

            steps {
                updateGitlabCommitStatus name: 'Deploy', state: 'running'
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
                updateGitlabCommitStatus name: 'Deploy', state: 'success'
            }

            post {
                failure {
                    updateGitlabCommitStatus name: 'Deploy', state: 'failed'
                }
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
