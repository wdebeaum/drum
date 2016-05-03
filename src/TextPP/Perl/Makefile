#
# Makefile -- install a set of Perl modules
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 24 Nov 2005
# Time-stamp: <Wed Jul 11 10:50:24 EDT 2007 ferguson>
# $Id: Makefile,v 1.3 2007/10/19 01:35:19 blaylock Exp $
#

# Will come from parent Makefile...
MODULE = MODULE

SRCS = Lingua/EN/Sentence.pm \
	Geo/StreetAddress/US.pm \
	AddressParser/AddressParser.pm \
	  TextPP/AbbrProc.pm \
	  TextPP/NumHack.pm \
	  TextPP/NumProc.pm \
	  TextPP/PuncProc.pm \
	  TextPP/toASCII.pm

CONFIGDIR=../../config
include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk

all default clean::
	@echo 'No need to make $@ in $(MODULE)/Perl'

install::
	@for src in $(SRCS); do \
	  destdir="$(etcdir)/$(MODULE)/`dirname $$src`"; \
	  (set -x;\
	   $(MKINSTALLDIRS) "$$destdir"; \
	   $(INSTALL_DATA) $$src "$$destdir"); \
	done
