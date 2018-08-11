#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

IMAGE=$DIR/debian_wheezy_mips_standard.qcow2

qemu-system-mips \
-M malta -kernel $DIR/vmlinux-3.2.0-4-4kc-malta \
-hda $IMAGE \
-append "root=/dev/sda1 console=tty0" \
-net user,hostfwd=tcp::10022-:22 -net nic
