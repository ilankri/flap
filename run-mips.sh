#!/bin/sh

$(dirname $0)/flap --gcc true --retromips true -s hopix -t mips $1 &&   \
    mv $(printf "%s/%s.%s" $(dirname $1) $(basename -s .hopix $1) mips) \
       /tmp/prog.S &&                                                   \
    mips-linux-gnu-gcc -g3 -static -mno-shared -o /tmp/prog             \
                       $(dirname $0)/lib/runtime.c /tmp/prog.S &&       \
    qemu-mips /tmp/prog
