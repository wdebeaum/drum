#!/bin/sh

# collect numbers from batch.lisps and counts.txts into one big table

for d in 62*
do perl -n -e 'chomp; print "$&\t" if (/[\d\.]+$/);' $d/batch.lisp
   perl -e 'undef $/; $_=<>; print join("\t",@{[/\d+/g]}) . "\n";' $d/counts.txt
done
