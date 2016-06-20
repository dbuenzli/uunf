The distribution contains generated data. If you want to contribute
please hack your way directly via the source repository.

For developing, you will need to install [uucd][1] and download a copy
of the XML Unicode character database to an absolute $DBPATH. From the
root directory of the repository type:

    ln -s $DBPATH support/ucd.xml
    ./build support

The result is in the file `src/uunf_data.ml`. It contains
the data extracted from the Unicode character database needed to
implement the normalization forms. This file is ignored by git.

For the `./build test` target to work download the `NormalizationTest.txt`
file to the `test` directory (e.g. using `./build download-test-files`). Then
simply type:

    ./build test

to run the tests.
 
[1] http://erratique.ch/software/uucd
