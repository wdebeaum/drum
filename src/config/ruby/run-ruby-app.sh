#!/bin/sh
#
# run-ruby-app.sh: Shell script wrapper for launching a Ruby app
#
# George Ferguson, ferguson@cs.rochester.edu, 27 Jul 2005
# Time-stamp: <Fri May  4 09:54:00 EDT 2007 ferguson>
#
# This file will be customized for a specific application by setting
# the variables at the top of the file. If this file's name is not
# "run-ruby-app.sh", then you are looking at such a customization.
#

# Variables set by component Makefile
MODULE=ModuleName
MAIN=main.pl

# Root of TRIPS installation
TRIPS_BASE_DEFAULT=.
if test -z "$TRIPS_BASE"; then
    TRIPS_BASE=$TRIPS_BASE_DEFAULT
    export TRIPS_BASE
fi

# Ruby interpreter
if test -z "$RUBY"; then
    RUBY=/usr/bin/ruby
fi

etcdir="$TRIPS_BASE/etc"
moduledir="$etcdir/$MODULE"

# Run Ruby
exec $RUBY -I"$moduledir" -I"$etcdir" "$moduledir/$MAIN" "$@"
