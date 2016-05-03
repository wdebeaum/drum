#
# Makefile for TRIPS KQML libraries
#
# George Ferguson, ferguson@cs.rochester.edu,  3 Sep 2003
# $Id: Makefile,v 1.10 2015/04/03 15:46:52 wdebeaum Exp $
#

CONFIGDIR=../config
include $(CONFIGDIR)/defs.mk

# In OSX, also build objc library
LANGUAGES = c java perl ruby javascript
# FIXME: 10.6 required
#\
#	    $(if $(findstring darwin,$(target)),objc)

test_for_language = test -f Makefile-$(1) -a -d $(CONFIGDIR)/$(1:objc=c)

all: $(LANGUAGES)

$(LANGUAGES):
	@if $(call test_for_language,$@); then $(MAKE) -f Makefile-$@; fi

clean install install.man:
	@for l in $(LANGUAGES); do \
	  if $(call test_for_language,$$l); then $(MAKE) -f Makefile-$$l $@; fi; \
	done
