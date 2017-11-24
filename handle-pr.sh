#!/bin/bash -e
ACTION=$1
PR=$2
REPO=$3
COMMIT=$4

echo handle pr action:$ACTION pr:$PR repo:$REPO commit:$COMMIT

[ -z $PR ] && exit
[ $ACTION != "closed" ] && exit

unset NAME
unset NODE

function create_deployment() {
	curl -s -X POST \
		-H "Content-Type: application/json" \
		-d "{ \"ref\": \"$COMMIT\", \"description\": \"Docker Deploy\", \"auto_merge\": false }" \
		"https://api.github.com/repos/$REPO/deployments?access_token=$TOKEN" | jq '.id'
}

function set_status() {
	curl -s -X POST \
		-H "Content-Type: application/json" \
		-d "{ \"state\": \"$2\", \"description\": \"Docker Deploy\" }" \
		"https://api.github.com/repos/$REPO/deployments/$1/statuses?access_token=$TOKEN"
}

if [ $REPO = "ezuce/reach3" ]
then
	NETWORK=pr-$PR
	docker stop $(docker ps -q -f "name=.$NETWORK")
	docker rm $(docker ps -a -q -f "name=.$NETWORK")
	docker rmi $(docker images -q -f "reference=$NETWORK/*")
fi

if [ $REPO = "ezuce/reach3" ]
then
	ID=$(create_deployment)
	set_status $ID pending
	cd ~/docker/reach3 && ./build.sh && ./run.sh && set_status $ID success || set_status $ID error
elif [ $REPO = "swarmcom/reach-ui" ]
then
	ID=$(create_deployment)
	set_status $ID pending
	cd ~/docker/reach-ui && ./build.sh && ./run.sh && set_status $ID success || set_status $ID error
fi
