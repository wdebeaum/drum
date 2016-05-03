#!/bin/bash

# make-terms-dot-txt.sh - make terms.txt from various different versions of NationalFile.zip

ZIPFILE=$1
if test -z "$ZIPFILE" ; then
  echo "Error: no zipfile provided (did you forget to install a NationalFile.zip and then run ./configure?)"
  exit 1
fi
VERSION=`echo "$ZIPFILE" |perl -n -e 'print $& if (/\d\d\d\d-\d\d\-\d\d/);'`
if test -z "$VERSION" ; then
  echo "Error: Can't find version of NationalFile.zip"
  exit 1
fi
echo "VERSION=$VERSION"

case $VERSION in
  2008-08-23)
    # This version of NationalFile.zip contains a text file that starts out
    # encoded as UTF-16 with a little-endian BOM, but at byte offset 10871742
    # switches to UTF-8 with no BOM (WTF?!).
    # That means I have to treat the first 10MB of the unzipped file
    # differently, using iconv to convert it from UTF-16 to UTF-8. A block size
    # of 42 seems big enough to be efficient, and it evenly divides the
    # required byte offset.
    unzip -p $ZIPFILE | \
    dd bs=42 count=258851 | \
    iconv -f UTF-16 -t UTF-8 | \
    LC_ALL='C' cut -d '|' -f 2 | \
    perl -n -e 's/ \(.*\)//; s/^\s*//; s/\s*$//; print "$_\n" unless (/Feature_Name/i or $_ eq "");' >terms.unsorted.txt || exit 1
    unzip -p $ZIPFILE | \
    dd bs=42 skip=258851 | \
    LC_ALL='C' cut -d '|' -f 2 | \
    perl -n -e 's/ \(.*\)//; s/^\s*//; s/\s*$//; print "$_\n" unless (/Feature_Name/i or $_ eq "");' >>terms.unsorted.txt || exit 1
    LC_ALL='C' sort <terms.unsorted.txt | LC_ALL='C' uniq >terms.txt || exit 1
    ;;
  2009-08-03|2009-06-01|2008-12-28)
    # These versions were separated into two files, one for Alaska and Hawaii
    # in UTF-16, and the other for everywhere else in UTF-8
    unzip -p $ZIPFILE NationalFile_AK_HI_20??????.txt | \
    iconv -f UTF-16 -t UTF-8 | \
    LC_ALL='C' cut -d '|' -f 2 | \
    perl -n -e 's/ \(.*\)//; s/^\s*//; s/\s*$//; print "$_\n" unless (/Feature_Name/i or $_ eq "");' >terms.unsorted.txt || exit 1
    unzip -p $ZIPFILE NationalFile_20??????.txt | \
    LC_ALL='C' cut -d '|' -f 2 | \
    perl -n -e 's/ \(.*\)//; s/^\s*//; s/\s*$//; print "$_\n" unless (/Feature_Name/i or $_ eq "");' >>terms.unsorted.txt || exit 1
    LC_ALL='C' sort <terms.unsorted.txt | LC_ALL='C' uniq >terms.txt || exit 1
    ;;
  *)
    # All versions from 2009-10-02 on.
    # This version goes back to a single file, but this time all UTF-8 (with a
    # BOM)
    unzip -p $ZIPFILE NationalFile_20??????.txt | \
    LC_ALL='C' cut -d '|' -f 2 | \
    perl -n -e 's/ \(.*\)//; s/^\s*//; s/\s*$//; print "$_\n" unless (/Feature_Name/i or $_ eq "");' >terms.unsorted.txt || exit 1
    LC_ALL='C' sort <terms.unsorted.txt | LC_ALL='C' uniq >terms.txt || exit 1
    ;;
esac

# sanity checks
if test \! -s terms.txt ; then
  echo "Error: terms.txt is empty"
  exit 1
fi
if grep -q -e '^$' terms.txt ; then
  echo "Error: terms.txt has a blank line in it"
  exit 1
fi

