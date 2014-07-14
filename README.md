modes-generator
===============
To use, install the following OCaml packages:

> core, cryptokit, dolog, ocamlfind (for compilation), ocamlgraph

Then run:

> ocaml setup.ml -configure
> ocaml setup.ml -build

To reproduce the results from the paper, run the following:

> ./mosynth.native -all -block-size 10 -valid-count -print-modes > valid
> ./mocheck.native -file valid -is-decryptable > decryptable
> ./mocheck.native -file decryptable -is-secure > secure

Please e-mail "amaloz at cs dot umd dot edu" if you encounter any issues.
