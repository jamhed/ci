#!/bin/sh -e
PR=$1
if [ -z $PR ]
then
	export NETWORK=ezuce
	REACH_BRANCH=master
else
	export NETWORK=pr-$PR
	REACH_BRANCH=pull/$PR/head
fi
unset NODE

cd ~/docker
docker network create $NETWORK || true

cd reach3 && BRANCH=$REACH_BRANCH ./build.sh && ./run.sh && cd ../

echo wait reach node to boot up
sleep 5

docker exec reach.$NETWORK ./rpc.sh db_param set b:fs_agent_gateway b:agents.$NETWORK
docker exec reach.$NETWORK ./rpc.sh db_param set b:fs_outgoing_gateway b:agents.$NETWORK
docker exec reach.$NETWORK ./rpc.sh db_param set b:call_record_template b:http_cache://http://rr.$NETWORK:9090/records/~s.wav

cd busytone && BRANCH=reach-rework ./build.sh && ./run.sh && cd ../

for component in freeswitch-reach3 agents reach-ui rr
do
	cd $component && ./build.sh && ./run.sh && cd ../
done
