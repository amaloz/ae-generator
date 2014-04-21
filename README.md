modes-generator
===============
To use, install the following OCaml packages:

> core, cryptokit, dolog, ocamlfind (for compilation), ocamlgraph

Then run:

> ./build.sh

To reproduce the results from the paper, run the following:

> ./mosynth.native -all -block-size 10 -valid-count
> ./mosynth.native -all -block-size 10 -decryptable-count
> ./mosynth.native -all -block-size 10
