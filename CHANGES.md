v1.0.0 2015-06-17 Cambridge (UK)
--------------------------------

- Updated for Unicode 8.0.0
- `topkg` support
- `Uunf.add` now eventually returns `` `End`` whenever the latter was
  encoded and the character stream was entirely output. In most existing
  programs this will simply entail to add `` `End`` to the existing
  `` `Await`` case in pattern matches on the result of `Uunf.add`.
- Adds the `Uunf_string` library that allows to directly normalize UTF-X
  OCaml encoded strings. This library depends on `Uutf`.
- Rewrote the utility `unftrip` to use `Cmdliner` which is now
  an optional dependency of the package. The cli interface is
  incompatible with previous versions. Support for random
  Unicode scalar value  generation was removed, use `utftrip` from
  the `Uutf` package for that.
- Rewrote the module's data generation to essentially match what is done
  in `Uucp`. Much less ugly, no source file `sed`ding.

v0.9.3 2014-06-16 Cambridge (UK)
--------------------------------

- Updated for Unicode 7.0.0

v0.9.2 2013-10-01 Lausanne
--------------------------

- Updated for Unicode 6.3.0
- OPAM friendly workflow and drop OASIS support.

v0.9.1 2013-01-04 La Forclaz (VS)
---------------------------------

- Updated for Unicode 6.2.0.
- Fix Uunf.is_scalar_value always returning false.
- Make the module completely safe for the client.
- Change command line help of unftrip.

v0.9.0 2012-09-07 Lausanne
--------------------------

First release.
