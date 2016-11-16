#!/usr/bin/env bash

for i in `mr list 2> /dev/null | grep 'mr' | grep '/' | cut -f2 -d: | cut -f2 -d' '`; do
  echo students/`basename $i`
done