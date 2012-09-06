#!/bin/sh
# usage: nf.sh NF < in > out

EXP="use Unicode::Normalize; while (\$line = <STDIN>){ print $1(\$line);}"
perl -CDS -e "$EXP"