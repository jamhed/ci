#!/bin/bash -e
REPO=$1
BRANCH=$2
[ -z $BRANCH ] && exit

unset NAME
unset NODE

if [ $REPO = "reach3" ] && [ $BRANCH = "refs/heads/master" ]
then
	docker exec reach.ezuce bash -c 'cd reach && git fetch && git reset --hard origin/master'
elif [ $REPO = "reach-ui" ] && [ $BRANCH = "refs/heads/jamhed-devel" ]
then
	cd ~/docker/reach-ui-jh && ./build.sh && ./run.sh
else
	echo skip $REPO $BRANCH
fi
