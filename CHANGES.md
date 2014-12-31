
- `Uunf.add` now returns ````End``` whenever the latter was encoded
  and there's character stream was entirely output. In most existing
  programs this will simply entail to add ```End``` to the existing
  ```Await``` case in pattern matches on the result of `Uunf.add`.

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
