#!/usr/bin/env sh

EXT="native"

FLAGS="-I src -use-ocamlfind -cflags -annot,-g"

ocamlbuild $FLAGS mosynth.$EXT
ocamlbuild $FLAGS mocheck.$EXT
