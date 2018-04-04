#!/bin/bash -e
REPO=$1
COMMIT=$3
BRANCH=$(sed -e 's/refs\/heads\///' $2)

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
		-d "{ \"state\": \"$2\", \"description\": \"Docker Deploy\", \"target_url\": \"https://docker.ezuce.com/ci/reports/$COMMIT\" }" \
		"https://api.github.com/repos/$REPO/deployments/$1/statuses?access_token=$TOKEN" | jq '.state'
}

if [ $REPO = "swarmcom/reach-ui" ] && [ $BRANCH = "refs/heads/jamhed-devel" ]
then
	ID=$(create_deployment)
	set_status $ID pending
	export NETWORK=devel
	export BRANCH=$BRANCH
	export HUB=$BRANCH
	export NAME=reach-ui-$BRANCH.$NETWORK
	cd ~/docker/reach-ui && ./build.sh && ./run.sh && set_status $ID success || set_status $ID error
else
	echo skip $REPO $BRANCH
fi
