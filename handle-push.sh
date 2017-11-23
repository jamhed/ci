#!/bin/bash -e
REPO=$1
BRANCH=$2
COMMIT=$3
URL=$4
[ -z $BRANCH ] && exit

unset NAME
unset NODE

function set_status() {
	echo set status $1
	[ -n $COMMIT ] && [ -n $URL ] && curl -s -X POST \
		-H "Content-Type: application/json" \
		-d "{ \"state\": \"$1\", \"context\": \"Docker Deploy\" }" \
		"$URL/statuses/$COMMIT?access_token=$TOKEN"
}

if [ $REPO = "reach3" ] && [ $BRANCH = "refs/heads/master" ]
then
	set_status pending
	cd ~/docker/reach3 && ./build.sh && ./run.sh && set_status ok
elif [ $REPO = "reach-ui" ] && [ $BRANCH = "refs/heads/jamhed-devel" ]
then
	set_status pending
	cd ~/docker/reach-ui-jh && ./build.sh && ./run.sh && set_status ok
else
	echo skip $REPO $BRANCH
fi
