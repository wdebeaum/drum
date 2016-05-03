#!/bin/sh

TMPFILE=`mktemp` || exit 1
./get-quick-look-preview.rb <$1 >>$TMPFILE
xpdf $TMPFILE
rm $TMPFILE

