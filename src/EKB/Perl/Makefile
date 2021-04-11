#
# Makefile -- install a set of Perl modules
#
# Lucian Galescu <lgalescu@ihmc.us> 2017/02/03
# $Id: Makefile,v 1.10 2021/04/11 03:47:15 lgalescu Exp $
#

# Will come from parent Makefile...
MODULE =

ifndef MODULE
$(error This Makefile should only be called from its parent)
endif

SRCS =  AKRL2EKB.pm \
	EKBAgent.pm \
	EKB.pm \
	StringParser.pm \
	$(wildcard AKRL/*.pm) \
	$(wildcard DRUM/*.pm) \
	$(wildcard EKB/*.pm) \
	$(wildcard EKB/Reasoner/*.pm) \
	$(wildcard Ont/*.pm)

SRCS_IN = \
	Ont/TRIPS.pm.in

SRCS_OUT = $(SRCS_IN:.in=)

SRCS += $(SRCS_OUT)

# we typically don't need to make in subdirs; exceptions are made via this makefile
# SUBDIRS = EKB Ont

CONFIGDIR=../../config
include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk

export TRIPS_BASE=$(prefix)

all default clean::
	@echo 'No need to make $@ in $(MODULE)/Perl'

install:: $(SRCS)
	@for src in $(SRCS); do \
	  destdir="$(etcdir)/$(MODULE)/`dirname $$src`"; \
	  (set -x;\
	   $(MKINSTALLDIRS) "$$destdir"; \
	   $(INSTALL_DATA) $$src "$$destdir"); \
	done

# this uses $TRIPS_BASE/src/DeepSemLex/data/TRIPS/trips-ont-dsl.xml
Ont/TRIPS.pm: Ont/TRIPS.pm.in | dsl
	../tools/make-ont-file.pl $< > $@

.PHONY: dsl
dsl:
	make -C $(TRIPS_BASE)/src/DeepSemLex/data/ TRIPS/COMPLETE
