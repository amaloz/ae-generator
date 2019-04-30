# `aesynth`: An authenticated encryption scheme prover/synthesizer

`aesynth` implements the algorithm presented in
<https://eprint.iacr.org/2015/624>. It comes in two modes: `aesynth check`
checks the security of a given mode, and `aesynth synth` synthesizes modes of
specific lengths.

## Installation Instructions

To build, you'll need either `opam` or `dune` installed.

### Building using `opam`

Run the following:

> opam install .

This installs `aesynth` to your `opam` install directory, and thus, assuming
that directory is in your path, you can directly execute `aesynth` from the
command line.

### Building using `dune`

Run the following:

> dune build -p aesynth

This won't install `aesynth` locally, and so to run `aesynth` you need to
execute the following:

> dune exec aesynth --

## License

Beerware

## Questions / Concerns

Please e-mail "amaloz at galois dot com" if you encounter any issues.
