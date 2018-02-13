#!/bin/sh

# trips-drum-pmc.sh
#
# Time-stamp: <Mon Feb 12 15:39:18 CST 2018 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 12 Feb 2018
#

# Script for running an instance of DRUM for processing an article from PMC

usage='trips-drum-pmc [-port 6200] -pmcid PMCID'


echo 'This is TRIPS/drum version 0'
TRIPS_BASE_DEFAULT=/usr/local/trips

# Set TRIPS_BASE unless set
if test ! -z "$TRIPS_BASE"; then
    echo "Using your TRIPS_BASE=\"$TRIPS_BASE\""
else
    TRIPS_BASE=$TRIPS_BASE_DEFAULT; export TRIPS_BASE
    echo "Using TRIPS_BASE=\"$TRIPS_BASE\""
fi

TRIPS_HOST_DEFAULT=localhost
TRIPS_PORT_DEFAULT=6200

#############################################################################
#
# Command-line

port=''
pmcid=''

while test ! -z "$1"; do
    case "$1" in
	-port)		port="$2";	shift;;
	-pmcid)		pmcid="$2";	shift;;
	-help|-h|-\?)
	    echo "usage: $usage"
	    exit 0;;
	*)
	    echo "$0: unknown argument: $1" 1>&2
	    echo "usage: $usage" 1>&2;
	    exit 1;;
    esac
    shift
done

if test -z "$pmcid"; then
    echo "No pmcid argument given!" 1>&2
    echo "usage: $usage" 1>&2
    exit 1
fi

# allow PMC prefix
pmcid=`echo $pmcid | perl -pe 's/^PMC//i'`
echo "Running PMCID: $pmcid" 1>&2

# set port options
TRIPS_PORT=${port:-$TRIPS_PORT_DEFAULT}
TRIPS_SOCKET=${TRIPS_HOST_DEFAULT}:${TRIPS_PORT}
export TRIPS_SOCKET

# start tripd-drum
${TRIPS_BASE}/bin/trips-drum -nouser -port $port 1>&2 &

# send message to process article
run_message="request :receiver DRUM :content (run-pmcid :pmcid \"$pmcid\" :exit-when-done true)"

sleep 20
${TRIPS_BASE}/bin/trips_msg -socket ${TRIPS_SOCKET} $run_message 1>&2 &

# Bye
exit 0
