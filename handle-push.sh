#!/bin/bash -e
REPO=$1
BRANCH=$2
[ -z $BRANCH ] && exit

unset NAME
unset NODE

if [ $REPO = "reach3" ] && [ $BRANCH = "refs/heads/master" ]
then
	docker exec reach.ezuce bash -c 'cd reach && git pull origin master'
else
	echo skip $REPO $BRANCH
fi
