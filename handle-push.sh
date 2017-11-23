#!/bin/bash -e
REPO=$1
BRANCH=$2
COMMIT=$3
[ -z $BRANCH ] && exit

unset NAME
unset NODE

function create_deployment() {
	[ -n $COMMIT ] && curl -s -X POST \
		-H "Content-Type: application/json" \
		-d "{ \"ref\": \"$COMMIT\", \"payload\": \"Docker Deploy\" }" \
		"https://api.github.com/repos/$REPO/deployments?access_token=$TOKEN" | jq '.id'
}

function set_success() {
	[ -n $COMMIT ] && curl -s -X POST \
		-H "Content-Type: application/json" \
		-d "{ \"state\": \"success\", \"context\": \"Docker Deploy\" }" \
		"https://api.github.com/repos/$REPO/deployments/$1/statuses?access_token=$TOKEN"
}

if [ $REPO = "ezuce/reach3" ] && [ $BRANCH = "refs/heads/master" ]
then
	ID=$(create_deployment)
	cd ~/docker/reach3 && ./build.sh && ./run.sh && set_success ID
elif [ $REPO = "swarmcom/reach-ui" ] && [ $BRANCH = "refs/heads/jamhed-devel" ]
then
	ID=$(create_deployment)
	cd ~/docker/reach-ui-jh && ./build.sh && ./run.sh && set_success ID
else
	echo skip $REPO $BRANCH
fi
