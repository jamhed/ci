#!/bin/bash -e
PR=$1
REPO=$2
[ -z $PR ] && exit

unset NAME
unset NODE

if [ $REPO = "reach3" ]
then
	NETWORK=pr-$PR
	docker stop $(docker ps -q -f "name=.$NETWORK")
	docker rm $(docker ps -a -q -f "name=.$NETWORK")
	docker rmi $(docker images -q -f "reference=$NETWORK/*")
fi

if [ $REPO = "reach3" ]
then
	cd ~/docker/reach3 && ./build.sh && ./run.sh
elif [ $REPO = "reach-ui" ]
then
	cd ~/docker/reach-ui && ./build.sh && ./run.sh
fi