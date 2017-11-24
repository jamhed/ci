#!/bin/bash -e
REPO=$1
BRANCH=$2
COMMIT=$3

echo handle push repo:$REPO branch:$BRANCH commit:$COMMIT

[ -z $BRANCH ] && exit

unset NAME
unset NODE

function create_deployment() {
	[ -n $COMMIT ] && curl -s -X POST \
		-H "Content-Type: application/json" \
		-d "{ \"ref\": \"$COMMIT\", \"description\": \"Docker Deploy\", \"auto_merge\": false }" \
		"https://api.github.com/repos/$REPO/deployments?access_token=$TOKEN" | jq '.id'
}

function set_status() {
	[ -n $COMMIT ] && curl -s -X POST \
		-H "Content-Type: application/json" \
		-d "{ \"state\": \"$2\", \"description\": \"Docker Deploy\" }" \
		"https://api.github.com/repos/$REPO/deployments/$1/statuses?access_token=$TOKEN"
}

if [ $REPO = "ezuce/reach3" ] && [ $BRANCH = "refs/heads/master" ]
then
	ID=$(create_deployment)
	set_status $ID pending
	cd ~/docker/reach3 && ./build.sh && ./run.sh && set_status $ID success || set_status $ID error
elif [ $REPO = "swarmcom/reach-ui" ] && [ $BRANCH = "refs/heads/jamhed-devel" ]
then
	ID=$(create_deployment)
	set_status $ID pending
	cd ~/docker/reach-ui-jh && ./build.sh && ./run.sh && set_status $ID success || set_status $ID error
else
	echo skip $REPO $BRANCH
fi
