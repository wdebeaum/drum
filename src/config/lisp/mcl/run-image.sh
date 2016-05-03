#!/bin/sh
#
# <FILENAME>: Launch OpenMCL with <MODULE> image
#
# George Ferguson, ferguson@cs.rochester.edu, 23 Mar 1999
# $Id: run-image.sh,v 1.1.1.1 2005/01/14 19:47:58 ferguson Exp $
#

CCL_DEFAULT_DIRECTORY=/usr/local/ccl
export CCL_DEFAULT_DIRECTORY

# Set TRIPS_BASE unless set
if test -z "$TRIPS_BASE"; then
    TRIPS_BASE=.
    export TRIPS_BASE
fi

if test -z "$LISP"; then
    LISP=$CCL_DEFAULT_DIRECTORY/dppccl
fi

IMAGE=lisp.image

# Exec the actual executable with original args, if any
exec $LISP $IMAGE ${1+"$@"}
