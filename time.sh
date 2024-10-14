#!/usr/bin/env bash

make $1

rm -f time_$1.dat

for i in {1024..16384..128}; do
  paste <(echo $i) <(./$1 $i | awk '{print $3}') \
    >> time_$1.dat
  echo "timing $i"
done

