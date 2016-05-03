#!/bin/sh
#
# <FILENAME>: Launch SBCL with <MODULE> image
#
# George Ferguson, ferguson@cs.rochester.edu, 2 Oct 2003
# $Id: run-image.sh,v 1.4 2016/03/15 05:17:45 lgalescu Exp $
#

# Set TRIPS_BASE unless set
if test -z "$TRIPS_BASE"; then
    TRIPS_BASE=.
    export TRIPS_BASE
fi

if test -z "$LISP"; then
    LISP=sbcl
fi

IMAGE=lisp.image

# show build date
BUILD_DATE=`ls -l $IMAGE | perl -pe 's,^.* ([a-z]+ +\d+ \d+:\d+) .*$,\1,i'`
echo TRIPS LISP core build date: $BUILD_DATE

# Exec the actual executable with original args, if any
# Feb 2004: Adding -batch and -quiet caused the dump to not get any messages
exec $LISP  --dynamic-space-size 4096 --noinform --core $IMAGE ${1+"$@"} --end-runtime-options --noprint
