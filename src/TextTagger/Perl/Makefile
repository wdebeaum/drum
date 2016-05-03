#
# Makefile -- install a set of Perl modules
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 24 Nov 2005
# Time-stamp: <Fri Aug 24 10:16:43 EDT 2012 ferguson>
# $Id: Makefile,v 1.40 2015/10/26 19:15:23 wdebeaum Exp $
#

SHELL=/bin/bash

# Will come from parent Makefile...
ifndef MODULE
$(error This Makefile should only be called from its parent)
endif

CONFIGDIR=../../config
include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/perl/defs.mk

srcs_pl=$(PERL) -n -e '$$optional = 1 if (/^\# all optional taggers/); print "TextTagger/$$1.pm " if (/^use TextTagger::(\w+);/ and $(1) $$optional);' TextTagger/Config.pm.in

AUTO_REQUIRED_SRCS = $(shell $(call srcs_pl,not))

REQUIRED_SRCS = \
	TextTagger.pm \
	TextTagger/Config.pm \
	Geo/StreetAddress/US.pm \
	Lingua/EN/Sentence.pm \
	$(AUTO_REQUIRED_SRCS)

$(info REQUIRED_SRCS=$(REQUIRED_SRCS))

OPTIONAL_SRCS = $(shell $(call srcs_pl,))

$(info OPTIONAL_SRCS=$(OPTIONAL_SRCS))

ifneq (,$(wildcard TextTagger/CJParser.pm))
ifeq (,$(TEXTTAGGER_cj_parser))
$(error Charniak-Johnson parser not configured)
endif
ifeq (,$(TEXTTAGGER_biomodel))
$(error Bio model for Charniak-Johnson parser not configured)
endif
endif

all:: $(REQUIRED_SRCS)

install:: $(REQUIRED_SRCS)
	@for src in $(REQUIRED_SRCS); do \
	  destdir="$(etcdir)/$(MODULE)/`dirname $$src`"; \
	  (set -x;\
	   $(MKINSTALLDIRS) "$$destdir"; \
	   $(INSTALL_DATA) $$src "$$destdir"); \
	done
	@for src in $(OPTIONAL_SRCS); do \
	  if test -e $$src; then \
	    destdir="$(etcdir)/$(MODULE)/`dirname $$src`"; \
	    (set -x;\
	     $(MKINSTALLDIRS) "$$destdir"; \
	     $(INSTALL_DATA) $$src "$$destdir"); \
	  fi \
	done

clean::
	rm -f TextTagger/Config.pm Geo Lingua

Geo/StreetAddress/US.pm:
	ln -s ../../TextPP/Perl/Geo

Lingua/EN/Sentence.pm:
	ln -s ../../TextPP/Perl/Lingua

TextTagger/Config.pm: TextTagger/Config.pm.in $(CONFIGDIR)/TextTagger/defs.mk $(shell ls $(OPTIONAL_SRCS) 2>/dev/null)
	(cd TextTagger ; ./make-config-dot-pm.pl) <$< >$@

# let make-config-dot-pm.pl check out missing required sources
$(AUTO_REQUIRED_SRCS): TextTagger/Config.pm

