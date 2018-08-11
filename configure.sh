#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

sshpass -p 'root' ssh -p 10022 root@localhost 'apt-get update ; apt-get install -y gcc gdb'
