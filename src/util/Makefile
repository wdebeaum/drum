#
# Makefile for TRIPS Utilities libraries
#
# George Ferguson, ferguson@cs.rochester.edu,  6 Nov 1995
# $Id: Makefile,v 1.3 2013/04/04 15:28:44 wdebeaum Exp $
#
# Something of a hack to have to check the ../config/$@ directory to see
# if this was part of our checkout...
#

LANGUAGES = c java lisp perl ruby

all: $(LANGUAGES)

$(LANGUAGES):
	@if test -f Makefile-$@ -a -d ../config/$@; then \
	  $(MAKE) -f Makefile-$@; \
	fi

compile: lisp

clean install install.man:
	@for l in $(LANGUAGES); do \
	  if test -f Makefile-$$l -a -d ../config/$$l; then \
	    $(MAKE) -f Makefile-$$l $@; \
	  fi; \
	done
