#
# Makefile for TripsModule
#
# George Ferguson, ferguson@cs.rochester.edu, 16 Feb 1998
# $Id: Makefile,v 1.10 2015/04/03 15:47:33 wdebeaum Exp $
#

LANGUAGES = java perl ruby javascript
CONFIGDIR = ../config

test_for_language = test -f Makefile-$(1) -a -d $(CONFIGDIR)/$(1:objc=c)

all: $(LANGUAGES)

$(LANGUAGES):
	@if $(call test_for_language,$@); then $(MAKE) -f Makefile-$@; fi

clean install install.man:
	@for l in $(LANGUAGES); do \
	  if $(call test_for_language,$$l); then $(MAKE) -f Makefile-$$l $@; fi; \
	done
