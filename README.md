Uunf — Unicode text normalization for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Uunf is an OCaml library for normalizing Unicode text. It supports all
Unicode [normalization forms][nf]. The library is independent from any
IO mechanism or Unicode text data structure and it can process text
without a complete in-memory representation.

Uunf has no dependency. It is distributed under the ISC license.

[nf]: http://www.unicode.org/reports/tr15/

Home page: http://erratique.ch/software/uunf  

## Installation

Uunf can be installed with `opam`:

    opam install uunf
    opam install uunf cmdliner uutf # for the unftrip tool.

If you don't use `opam` consult the [`opam`](opam) file for build
instructions and a complete specification of the dependencies.


## Documentation

The documentation and API reference can be consulted [online][doc] or
via `odig doc uunf`.

[doc]: http://erratique.ch/software/uunf/doc/


## Sample programs

If you installed Uuseg with `opam` sample programs are located in
the directory `opam var uunf:doc`.

A few test programs are in the `test` directory of the distribution.

- `test.native` tests the library with the Unicode Normalization Test
  file available from:

  http://www.unicode.org/Public/%%UNICODEVERSION%%/ucd/NormalizationTest.txt

  Nothing should fail.

- `test_string.native` tests the UTF-X OCaml string support.

- `unftrip.native` inputs Unicode text on `stdin` and rewrites it on
  `stdout` in a given normalization form. Invoke with `--help` for more
  information. Depends on [`uutf`](http://erratique.ch/software/uutf)
  and [`cmdliner`](http://erratique.ch/software/cmdliner).
