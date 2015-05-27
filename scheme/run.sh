#!/usr/bin/bash

MHz=2900
DIR="results"
NUM=100

function run {
    mkdir $DIR

    gcc -O3 -march=native timing_x86.c ocb.c -D$1 -D$2

    # sleep 1

    for i in $(seq 1 $NUM)
    do
        ./a.out $MHz $DIR/$i
    done
    inp='['
    for file in $(ls $DIR/*)
    do
        tmp=$(tail -1 $file | tr -s " " | cut -d " " -f 3)
        inp=$(echo "$inp $tmp, ")
    done
    inp=$(echo "$inp]")
    result=$(echo "\"$inp\"" | xargs python confidence.py)
    echo -e "$1\t$2\t$result"
    find $DIR -delete
}

run OCB_SCHEME   ENCRYPT
run OCB_SCHEME   DECRYPT
run NEW_1_SCHEME ENCRYPT
run NEW_1_SCHEME DECRYPT
run NEW_2_SCHEME ENCRYPT
run NEW_2_SCHEME DECRYPT
run NEW_3_SCHEME ENCRYPT
run NEW_3_SCHEME DECRYPT
