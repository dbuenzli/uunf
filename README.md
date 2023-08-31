Uunf — Unicode text normalization for OCaml
===========================================
%%VERSION%%

Uunf is an OCaml library for normalizing Unicode text. It supports all
Unicode [normalization forms]. The library is independent from any IO
mechanism or Unicode text data structure and it can process text
without a complete in-memory representation.

Uunf is distributed under the ISC license. It has no dependency.

[normalization forms]: http://www.unicode.org/reports/tr15/

Homepage: <http://erratique.ch/software/uunf>


## Installation

Uunf can be installed with `opam`:

    opam install uunf
    opam install uunf cmdliner uutf  # For the unftrip tool

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.


## Documentation

The documentation can be consulted [online] or via `odig doc uunf`.

Questions are welcome but better asked on the [OCaml forum] than on 
the issue tracker.

[doc]: http://erratique.ch/software/uunf/doc/
[OCaml forum]: https://discuss.ocaml.org/


## Sample programs

The [`unftrip`] tool normalises text provided on standard input.

See also the [doc examples].

[`unftrip`]: test/unftrip.ml
[doc examples]: test/examples.ml
