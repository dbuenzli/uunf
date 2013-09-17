The distribution contains generated data. If you want to contribute
please hack your way directly via the source repository.

For developing, you will need to install [uucd][1] and download a copy
of the XML Unicode character database to $DBPATH. From the root
directory of the repository type:

    ln -s $DBPATH data/ucd.xml
    ./build data

The result is in `data/data.ml`, it contains the definitions from
[`data/defs.ml`](data/defs.ml) and the data extracted from the Unicode
character database via the program
[`data/extract_data.ml`](data/extract_data.ml).

The file `data/data.ml` is ignored by git. During the construction of
the distribution by the script `pkg-distrib`, the data file is
generated and its content is directly substituted in `src/uunf.ml`,
see the script
[`pkg/hook-pkg-distrib-pre-build`](`pkg/hook-pkg-distrib-pre-build`).

For the `./build test` target to work download the
`NormalizationTest.txt` file from the Unicode character database to
$TESTPATH and from the root directory of the repository type:

    ln -s $TESTPATH data/test.txt
    ./build test


[1] http://erratique.ch/software/uucd
