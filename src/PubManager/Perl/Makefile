#
# Makefile -- install a set of Perl modules
#
# Author: Roger Carff <rcarff@ihmc.us>, 19 Dec 2016
# $Id: Makefile,v 1.2 2017/01/12 17:56:21 lgalescu Exp $
#

# Will come from parent Makefile...
MODULE = MODULE

SRCS = PubManager.pm \
	PubManager/Utils.pm \
	PubManager/Log.pm

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
