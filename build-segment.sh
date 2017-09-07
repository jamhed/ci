#!/bin/sh -e
PR=$1
if [ -z $PR ]
then
	echo Usage: $0 PR_NUMBER
	exit
fi
export NETWORK=pr-$1
unset NODE

cd ~/docker
docker network create $NETWORK || true

cd reach3 && BRANCH=pull/$PR/head ./build.sh && ./run.sh && cd ../

echo wait reach node to boot up
sleep 5

docker exec reach.$NETWORK ./rpc.sh db_param set b:fs_agent_gateway b:agents.$NETWORK
docker exec reach.$NETWORK ./rpc.sh db_param set b:fs_outgoing_gateway b:agents.$NETWORK

cd busytone && BRANCH=reach-rework ./build.sh && ./run.sh && cd ../

for component in freeswitch-reach3 agents reach-ui rr
do
	cd $component && ./build.sh && ./run.sh && cd ../
done
