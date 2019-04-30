#!/usr/bin/bash

for i in {12..15}
do
    echo "Size = $i"
    time timeout 6h dune exec aesynth -- synth -decode -simple -print -size $i -attack > $i-attack.txt
done

cat 15-attack.txt 14-attack.txt 13-attack.txt 12-attack.txt > tmp.txt
dune exec aesynth -- check -simple -dec-file tmp.txt -attack > found-modes-attack.txt
rm tmp.txt
