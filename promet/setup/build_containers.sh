#!/bin/bash
. ./promet/setup/build-tools/setup_enviroment.sh

CI_BUILD_TOKEN=sR7hRWM6e6nky9wNYt8-
API_BUILD_TOKEN=f1wdzddUx8y5yvNTym1-

cd /tmp
rm Dockerfile*
wget http://192.168.177.120:10080/promet/promet-client-docker/raw/master/Dockerfile.template

Content=$(cat ./Dockerfile.template | sed -e "s/PVERSION/$BUILD_VERSION/g" | sed -e "s/RANDOMNUMBER/$RANDOM/g")
Content=${Content//\\/\\\\} # \
#Content=${Content//\//\\\/} # /
Content=${Content//\'/\\\'} # ' (not strictly needed ?)
Content=${Content//\"/\\\"} # "
Content=${Content//   /\\t} # \t (tab)
Content=${Content//
/\\\n} # \n (newline)
Content=${Content//^M/\\\r} # \r (carriage return)
Content=${Content//^L/\\\f} # \f (form feed)
Content=${Content//^H/\\\b} # \b (backspace)

PAYLOAD=$(cat << 'JSON'
{
  "branch_name": "master",
  "commit_message": "Patch Version in Dockerfile",
  "actions": [
    {
      "action": "update",
      "file_path": "Dockerfile",
      "content": "
JSON
)
echo $PAYLOAD$Content'"}]}' > Dockerfile.tmp

curl -vv --request POST --header "PRIVATE-TOKEN: $API_BUILD_TOKEN" --header "Content-Type: application/json" --data @Dockerfile.tmp http://192.168.177.120:10080/api/v3/projects/114/repository/commits
rm Dockerfile*
wget http://192.168.177.120:10080/promet/promet-server-docker/raw/master/Dockerfile.template

Content=$(cat ./Dockerfile.template | sed -e "s/PVERSION/$BUILD_VERSION/g" | sed -e "s/RANDOMNUMBER/$RANDOM/g")
Content=${Content//\\/\\\\} # \
#Content=${Content//\//\\\/} # /
Content=${Content//\'/\\\'} # ' (not strictly needed ?)
Content=${Content//\"/\\\"} # "
Content=${Content//   /\\t} # \t (tab)
Content=${Content//
/\\\n} # \n (newline)
Content=${Content//^M/\\\r} # \r (carriage return)
Content=${Content//^L/\\\f} # \f (form feed)
Content=${Content//^H/\\\b} # \b (backspace)

PAYLOAD=$(cat << 'JSON'
{
  "branch_name": "master",
  "commit_message": "Patch Version in Dockerfile",
  "actions": [
    {
      "action": "update",
      "file_path": "Dockerfile",
      "content": "
JSON
)
echo $PAYLOAD$Content'"}]}' > Dockerfile.tmp

curl -vv --request POST --header "PRIVATE-TOKEN: $API_BUILD_TOKEN" --header "Content-Type: application/json" --data @Dockerfile.tmp http://192.168.177.120:10080/api/v3/projects/147/repository/commits

rm Dockerfile*
wget http://192.168.177.120:10080/promet/promet-server-docker-armhf/raw/master/Dockerfile.template

Content=$(cat ./Dockerfile.template | sed -e "s/PVERSION/$BUILD_VERSION/g" | sed -e "s/RANDOMNUMBER/$RANDOM/g")
Content=${Content//\\/\\\\} # \
#Content=${Content//\//\\\/} # /
Content=${Content//\'/\\\'} # ' (not strictly needed ?)
Content=${Content//\"/\\\"} # "
Content=${Content//   /\\t} # \t (tab)
Content=${Content//
/\\\n} # \n (newline)
Content=${Content//^M/\\\r} # \r (carriage return)
Content=${Content//^L/\\\f} # \f (form feed)
Content=${Content//^H/\\\b} # \b (backspace)

PAYLOAD=$(cat << 'JSON'
{
  "branch_name": "master",
  "commit_message": "Patch Version in Dockerfile",
  "actions": [
    {
      "action": "update",
      "file_path": "Dockerfile",
      "content": "
JSON
)
echo $PAYLOAD$Content'"}]}' > Dockerfile.tmp

curl -vv --request POST --header "PRIVATE-TOKEN: $API_BUILD_TOKEN" --header "Content-Type: application/json" --data @Dockerfile.tmp http://192.168.177.120:10080/api/v3/projects/159/repository/commits

rm Dockerfile*
wget http://192.168.177.120:10080/promet/promet-server-alpine-docker/raw/master/Dockerfile.template

Content=$(cat ./Dockerfile.template | sed -e "s/PVERSION/$BUILD_VERSION/g" | sed -e "s/RANDOMNUMBER/$RANDOM/g")
Content=${Content//\\/\\\\} # \
#Content=${Content//\//\\\/} # /
Content=${Content//\'/\\\'} # ' (not strictly needed ?)
Content=${Content//\"/\\\"} # "
Content=${Content//   /\\t} # \t (tab)
Content=${Content//
/\\\n} # \n (newline)
Content=${Content//^M/\\\r} # \r (carriage return)
Content=${Content//^L/\\\f} # \f (form feed)
Content=${Content//^H/\\\b} # \b (backspace)

PAYLOAD=$(cat << 'JSON'
{
  "branch_name": "master",
  "commit_message": "Patch Version in Dockerfile",
  "actions": [
    {
      "action": "update",
      "file_path": "Dockerfile",
      "content": "
JSON
)
echo $PAYLOAD$Content'"}]}' > Dockerfile.tmp

curl -vv --request POST --header "PRIVATE-TOKEN: $API_BUILD_TOKEN" --header "Content-Type: application/json" --data @Dockerfile.tmp http://192.168.177.120:10080/api/v3/projects/160/repository/commits
