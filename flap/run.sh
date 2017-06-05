#!/usr/bin/env bash

PROG=$1
MIPS=`echo $1 | sed s/hopix/mips/`
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

$DIR/flap --gcc true --retromips true -s hopix -t mips $1 && \
sshpass -p 'root' scp -P 10022 $MIPS root@localhost:prog.S && \
sshpass -p 'root' ssh -p 10022 root@localhost 'gcc -o prog runtime.o prog.S && ./prog'
