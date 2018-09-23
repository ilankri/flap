#!/bin/sh

quiet() {
    $@ > /dev/null
}

CONTAINER=$(docker run -td flap)
docker cp $2 $CONTAINER:/tmp/prog.hopix
docker exec $CONTAINER ./run$1.sh /tmp/prog.hopix
status=$?
$(quiet docker stop $CONTAINER)
$(quiet docker rm $CONTAINER)
return $status
