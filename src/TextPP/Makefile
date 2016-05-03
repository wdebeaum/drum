#
# Makefile for TextPP (in Perl)
#
# Lucian Galescu <lgalescu@ihmc.us> 2005/12/07
# $Id: Makefile,v 1.2 2007/07/16 13:51:02 lgalescu Exp $

MODULE = TextPP
MAIN = TextPP.pl
SRCS = TextPP.pl

SUBDIRS = Perl

CONFIGDIR=../config
include $(CONFIGDIR)/perl/prog.mk

all default clean install:: 
	@for d in $(SUBDIRS); do \
	  if test -d $$d; then \
	    (cd $$d; $(MAKE) $@ MODULE=$(MODULE)); \
	  fi; \
	done
