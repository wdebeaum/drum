#
# Makefile for config/lisp/defsystem
#
# George Ferguson, ferguson@cs.rochester.edu, 12 Jan 2004
# Time-stamp: <Fri Dec 10 10:23:58 EST 2010 ferguson>
#

include ../defs.mk
include ../$(LISP_FLAVOR)/defs.mk

DEFSYSTEM_DIR = defsystem-3.6i

all:
	$(call compile-file,$(DEFSYSTEM_DIR)/defsystem.lisp)

distclean::
	rm -f $(DEFSYSTEM_DIR)/defsystem.$(FASL)
