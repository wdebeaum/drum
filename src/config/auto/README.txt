#
# README.txt for auto
#
# George Ferguson, ferguson@cs.rochester.edu,  6 Jun 2002
# $Id: README.txt,v 1.1.1.1 2005/01/14 19:47:58 ferguson Exp $
#

This directory contains two things:

1. The auxiliary files used by autoconf and friends, which come from
   the autoconf distribution and are freely redistributable. These
   files are:
	depcomp
	install-sh
	missing
	mkinstalldirs

2. The Makefile "rules.mk", which defines targets useful with
   autoconf. It does nothing about building and installing, but
   provides targets for reconfiguring (based on autoconf documentation
   example) and cleaning. This file is included by all the Makefiles
   in the TRIPS config directory.
