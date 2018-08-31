#!/bin/sh


if [ $# -eq 1 ]; then
    rlwrap ./flap -i true -V true -VV true -s $1
else
    rlwrap ./flap -i true -V true -VV true -s $1 -t $2
fi
