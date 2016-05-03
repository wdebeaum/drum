#!/bin/sh
#
# run-perl-app.sh: Shell script wrapper for launching a Perl app
#
# George Ferguson, ferguson@cs.rochester.edu, 27 Jul 2005
# Time-stamp: <Fri May  4 09:54:00 EDT 2007 ferguson>
#
# This file will be customized for a specific application by setting
# the variables at the top of the file. If this file's name is not
# "run-perl-app.sh", then you are looking at such a customization.
#

# Variables set by component Makefile
MODULE=ModuleName
MAIN=main.pl
PERL_FLAGS=

# Root of TRIPS installation
TRIPS_BASE_DEFAULT=.
if test -z "$TRIPS_BASE"; then
    TRIPS_BASE=$TRIPS_BASE_DEFAULT
    export TRIPS_BASE
fi

# Perl interpreter
if test -z "$PERL"; then
    PERL=/usr/bin/perl
fi

etcdir="$TRIPS_BASE/etc"
moduledir="$etcdir/$MODULE"

# Run Perl
exec $PERL $PERL_FLAGS -I"$moduledir" -I"$etcdir" "$moduledir/$MAIN" "$@"
