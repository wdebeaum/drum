#!/bin/sh
#
# <FILENAME>: Launch Allegro CL with <MODULE> image
#
# George Ferguson, ferguson@cs.rochester.edu, 23 Mar 1999
# Time-stamp: <Mon Mar 25 15:19:14 EST 2002 myros>
#
# This is for use with ACL >= 5.0 (separate runtime and image files).
#
# As of ACL6.0, we can't use a generic lisp bootstrap executable with
# our dumped image. You have to use the one dumped along with the image.
# Check that. You can use it, but it has to have the same name as the image,
# so now way to save (oh well, it's only 22KB).
# Also, -Q no longer has any purpose.
#
# As of TRIPS-5.5, we have to stop using applications and go back to using
# dumped images, since applications can't have the compiler in them, and
# this kills the performance of modules that use the Prolog engine. Sigh.
#

# Set TRIPS_BASE unless set
if test -z "$TRIPS_BASE"; then
    TRIPS_BASE=.
    export TRIPS_BASE
fi

if test -z "$LISP"; then
    LISP=cll
fi

IMAGE=lisp.dxl

# Exec the actual executable with original args, if any
exec $LISP -I $IMAGE -- -transport "socket-server" ${1+"$@"}
