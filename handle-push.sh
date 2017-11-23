#!/bin/bash -e
REPO=$1
BRANCH=$2
COMMIT=$3
[ -z $BRANCH ] && exit

unset NAME
unset NODE

function set_status() {
	echo set status $1
	[ -n $COMMIT ] && curl -s -X POST \
		-H "Content-Type: application/json" \
		-d "{ \"state\": \"$1\", \"context\": \"Docker Deploy\" }" \
		"https://api.github.com/repos/$REPO/statuses/$COMMIT?access_token=$TOKEN"
}

if [ $REPO = "ezuce/reach3" ] && [ $BRANCH = "refs/heads/master" ]
then
	set_status pending
	cd ~/docker/reach3 && ./build.sh && ./run.sh && set_status success
elif [ $REPO = "swarmcom/reach-ui" ] && [ $BRANCH = "refs/heads/jamhed-devel" ]
then
	set_status pending
	cd ~/docker/reach-ui-jh && ./build.sh && ./run.sh && set_status success
else
	echo skip $REPO $BRANCH
fi
