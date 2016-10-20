#!/bin/bash
. ./promet/setup/build-tools/setup_enviroment.sh

CI_BUILD_TOKEN=sR7hRWM6e6nky9wNYt8

cd /tmp
rm -R promet-client-docker
git clone http://gitlab-ci-token:$CI_BUILD_TOKEN@192.168.177.120:10080/promet/promet-client-docker.git
cd promet-client-docker

Content=$(cat ./Dockerfile.template | sed -e "s/VERSION/$BUILD_VERSION/g")

PAYLOAD=$(cat << 'JSON'
{
  "branch_name": "master",
  "commit_message": "Automatic updated Version",
  "actions": [
    {
      "action": "update",
      "file_path": "Dockerfile",
      "content": "$Content"
    }
  ]
}
JSON
)
curl --request POST --header "PRIVATE-TOKEN: $CI_BUILD_TOKEN" --header "Content-Type: application/json" --data "$PAYLOAD" http://192.168.177.120:10080/api/v3/projects/114/repository/commits
