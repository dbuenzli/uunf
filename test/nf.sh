#!/bin/sh
# usage: perlnf.sh NF < in > out

EXP="use Unicode::Normalize; while (\$line = <STDIN>){ print $1(\$line);}"
perl -CDS -e "$EXP"