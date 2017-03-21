#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

sshpass -p 'root' scp -P 10022 $DIR/../lib/runtime.c root@localhost:
sshpass -p 'root' ssh -p 10022 root@localhost 'gcc -c runtime.c'

