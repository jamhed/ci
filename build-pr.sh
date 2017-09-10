#!/bin/bash -e
PR=$1
COMMIT=$2
if [ -z $PR ]
then
	export NETWORK=ezuce
	REACH_BRANCH=master
else
	export NETWORK=pr-$PR
	REACH_BRANCH=pull/$PR/head
fi
unset NODE
unset NAME
# supress versbose build log messages
export BUILD_FLAGS=-q

echo BUILD PR:$PR COMMIT:$COMMIT

function set_status() {
	echo set status $1
	[ -n $COMMIT ] && curl -s -X POST \
		-H "Content-Type: application/json" \
		-d "{ \"state\": \"$1\", \"target_url\": \"https://docker.ezuce.com/ci/pr/$PR\", \"context\": \"ReachCI\" }" \
		"https://api.github.com/repos/ezuce/reach3/statuses/$COMMIT?access_token=$TOKEN"
}

cd ~/docker
docker network create $NETWORK || true
set_status pending

cd reach3 && BRANCH=$REACH_BRANCH ./build.sh && ./run.sh && cd ../

echo wait reach node to boot up
sleep 5

docker exec reach.$NETWORK ./rpc.sh db_param set b:fs_agent_gateway b:agents.$NETWORK
docker exec reach.$NETWORK ./rpc.sh db_param set b:fs_outgoing_gateway b:agents.$NETWORK
docker exec reach.$NETWORK ./rpc.sh db_param set b:call_record_template b:http_cache://http://rr.$NETWORK:9090/records/~s.wav

cd busytone && BRANCH=reach-rework ./build.sh && ./run.sh && cd ../
# lock freeswitch master branch version
cd freeswitch-reach3 && COMMIT=2362cb4e58985579cce88b1b81479b0f4cfcb2c1 ./build.sh && ./run.sh && cd ../

for component in agents reach-ui rr
do
	cd $component && ./build.sh && ./run.sh && cd ../
done

echo wait fs to boot up
sleep 15

echo running test suite
docker exec busytone.$NETWORK ./rpc.sh test_sup run

[ -z $PR ] && exit

RUNNING=$(docker ps | grep -c $NETWORK) && true
SUCCESS=$(grep -c ',ok}' ~/pr/$PR) && true
FAILURE=$(grep -c 'not_ok}' ~/pr/$PR) && true
if [ "$RUNNING" = "6" ]
then
	if [ "$FAILURE" = "0" ] && [ "$SUCCESS" -gt "0" ]
	then
		set_status success
	else
		set_status error
	fi
else
	set_status failure
fi
echo SUCCESS:$SUCCESS FAILURE:$FAILURE
