The distribution contains generated data. If you want to contribute
please hack your way directly via the source repository.

For developing, you will need to install [uucd][uucd] and download a copy
of the XML Unicode character database to `support/ucd.xml` (this will be done
automatically if the file doesn't exist). From the root directory of the
repository type:

    ocaml ./pkg/build_support.ml

The result is in the file `src/uunf_data.ml`. It contains the data
extracted from the Unicode character database needed to implement the
normalization forms. This file is ignored by git.

[uucd]: http://erratique.ch/software/uucd

# New unicod release 

For now proceed as above to generate the data filees using an
up-to-date [uucd] package.

Update the opam file with: 

```
b0 cmd .opam.file > opam
```

# Reference tests 

To download the reference normalization tests for the version
mentioned in `B0.ml` to the `test` directory issue:

    b0 cmd download-tests

You can then check them with: 

    b0 -a test
 

