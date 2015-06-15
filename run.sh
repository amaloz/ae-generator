#!/usr/bin/bash

for i in {12..16}
do
    echo "Size = $i"
    time timeout 3h ./main.native synth -simple -print -size $i > $i.txt
done

cat 16.txt 15.txt 14.txt 13.txt 12.txt > found-modes.txt
