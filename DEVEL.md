The distribution contains generated data. If you want to contribute
please hack your way directly via the source repository.

For developing, you will need to install [uucd][1] and download a copy
of the XML Unicode character database to an absolute $DBPATH. From the
root directory of the repository type:

    ln -s $DBPATH data/ucd.xml
    ./build data

The result is in `data/data.ml`, it contains the definitions from
[`data/defs.ml`](data/defs.ml) and the data extracted from the Unicode
character database via the program
[`data/extract_data.ml`](data/extract_data.ml). This file is ignored
by git. 

Whenever a distribution is done with `pkg-distrib` of
[pkgopkg][2]. The hook 
[`pkg/hook-pkg-distrib-pre-build`](pkg/hook-pkg-distrib-pre-build) is
invoked. This hook builds the data file by invoking `./build data` 
and substitutes its content in `src/uunf.ml` by invoking 
`./build substdata`. See the corresponding targets in the
[`build`](build) script. 

In case of a git opam package build the script
[`pkg/hook-pkg-git`](pkg/hook-pkg-git) is invoked. This script
downloads the Unicode character database, generates and substitutes
the data file.

For the `./build test` target to work download the
`NormalizationTest.txt` file from the Unicode character database to an
absolute $TESTPATH and from the root directory of the repository type:

    ln -s $TESTPATH data/test.txt
    ./build test

[1] http://erratique.ch/software/uucd
[2] https://github.com/dbuenzli/pkgopkg
