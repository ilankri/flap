#!/bin/sh

quiet() {
    $@ > /dev/null
}

CONTAINER=$(docker run -td flap)
docker cp $1 $CONTAINER:/tmp/prog.hopix
docker exec $CONTAINER ./run.sh lib/runtime.c /tmp/prog.hopix
status=$?
$(quiet docker stop $CONTAINER)
$(quiet docker rm $CONTAINER)
return $status
