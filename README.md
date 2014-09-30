modes-generator
===============
To use, install the following OCaml packages:

> core, cryptokit, dolog, ocamlfind (for compilation), ocamlgraph

Then run:

> (optional) oasis setup

> ocaml setup.ml -configure

> ocaml setup.ml -build

To reproduce the results from the paper, run the following:

> ./mosynth.native -all -block-size 10 -valid-count

> ./mosynth.native -all -block-size 10 -decryptable-count

> ./mosynth.native -all -block-size 10

Please e-mail "amaloz at cs dot umd dot edu" if you encounter any issues.
