#!/bin/sh
#
# <FILENAME>: Launch CMUCL with <MODULE> image
#
# George Ferguson, ferguson@cs.rochester.edu, 2 Oct 2003
# $Id: run-image.sh,v 1.1.1.1 2005/01/14 19:47:58 ferguson Exp $
#

# Set TRIPS_BASE unless set
if test -z "$TRIPS_BASE"; then
    TRIPS_BASE=.
    export TRIPS_BASE
fi

if test -z "$LISP"; then
    LISP=cmucl
fi

IMAGE=lisp.image

# Exec the actual executable with original args, if any
# Feb 2004: Adding -batch and -quiet caused the dump to not get any messages
exec $LISP -nositeinit -noinit -core $IMAGE ${1+"$@"}
