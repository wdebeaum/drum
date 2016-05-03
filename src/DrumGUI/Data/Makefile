#
# Makefile
#
# Time-stamp: <Thu Feb 12 14:58:39 CST 2015 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  9 Feb 2010
#

CONFIGDIR=../../config
include $(CONFIGDIR)/defs.mk

# add subdirs where we need to make 
SUBDIRS = eval-samples

INSTALL_DIR = $(etcdir)/Data

ifndef SUBDIRS

all clean::
	@echo 'No need to "make $@" in' `pwd`

else

all clean::
	@for d in $(SUBDIRS); do \
	  if test -d $$d; then \
	    (cd $$d; $(MAKE) $@); \
	  fi; \
	done

endif

install:
	$(MKINSTALLDIRS) $(DATA_DIR)
	rsync -Cau --exclude Makefile --exclude ".*" . $(INSTALL_DIR)

