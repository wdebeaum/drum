#!/bin/sh
#
# <FILENAME>: Launch Allegro CL with <MODULE> image
#
# George Ferguson, ferguson@cs.rochester.edu, 23 Mar 1999
# $Id: run-image.sh,v 1.3 2008/09/14 18:25:58 wdebeaum Exp $
#

# Set TRIPS_BASE unless set
if test -z "$TRIPS_BASE"; then
    TRIPS_BASE=.
    export TRIPS_BASE
fi

if test -z "$LISP"; then
    LISP=lisp
fi

IMAGE=lisp.dxl
TARGET=target

if test "$TARGET" = "i686-pc-cygwin"; then
    IMAGE1=`cygpath -w $IMAGE`
else
    IMAGE1=$IMAGE
fi

# Exec the actual executable with original args, if any
exec $LISP -I $IMAGE1 -q -batch ${1+"$@"}
