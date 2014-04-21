#!/usr/bin/env sh

EXT="native"

# Z3PATH=/home/amaloz/Desktop/z3/build
# export OCAMLPATH=$Z3PATH/api/ml

# FLAGS="-I src -use-ocamlfind -lflags -cclib,-lz3 -cflags -annot"
FLAGS="-I src -use-ocamlfind -cflags -annot,-g"

ocamlbuild $FLAGS mosynth.$EXT
ocamlbuild $FLAGS mocheck.$EXT
