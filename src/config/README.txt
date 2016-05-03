#
# README for config
#
# George Ferguson, ferguson@cs.rochester.edu,  6 Jun 2002
# $Id: README.txt,v 1.1.1.1 2005/01/14 19:47:58 ferguson Exp $
#

This directory contains code to configure the TRIPS system prior to
building it from source.

If the "configure" script is not present in this directory, or if it
or the "configure" scripts in the subdirectories need to be rebuilt,
they can be recreated by running "autoreconf". This is roughly
equivalent to running "autoconf" in each subdir, but slower because it
also runs other GNU build system apps that we don't use.

The "INSTALL.txt" file contains the generic build instructions
provided with "autoconf", which describe how to invoke the
"configure" script.

For a description of the available configuration options, run:

    ./configure --help=recursive
