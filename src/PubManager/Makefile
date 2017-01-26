#
# Makefile for PubManager (in Perl)
#
# Roger Carff <rcarff@ihmc.us> 2016/12/19
# $Id: Makefile,v 1.1 2017/01/06 20:06:19 rcarff Exp $

MODULE = PubManager
MAIN = PubManager.pl
SRCS = PubManager.pl

SUBDIRS = Perl

CONFIGDIR=../config
include $(CONFIGDIR)/perl/prog.mk

all default clean install:: 
	@for d in $(SUBDIRS); do \
	  if test -d $$d; then \
	    (cd $$d; $(MAKE) $@ MODULE=$(MODULE)); \
	  fi; \
	done
