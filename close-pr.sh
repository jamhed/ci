#!/bin/bash -e
PR=$1
[ -z $PR ] && exit
NETWORK=pr-$PR

docker stop $(docker ps -q -f "name=.$NETWORK")
docker rm $(docker ps -a -q -f "name=.$NETWORK")
docker rmi $(docker images -q -f "reference=$NETWORK/*")