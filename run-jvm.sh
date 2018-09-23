#!/bin/sh

$(dirname $0)/flap -s hopix -t jakix $1 &&                              \
    mv $(printf "%s/%s.%s" $(dirname $1) $(basename -s .hopix $1) k)    \
       /tmp/prog.j &&                                                   \
    jasmin /tmp/prog.j &&                                               \
    java -noverify Flap
