#
# Makefile -- install a set of Perl modules
#
# Lucian Galescu <lgalescu@ihmc.us> 2017/02/03
# $Id: Makefile,v 1.8 2020/08/19 20:25:53 lgalescu Exp $
#

# Will come from parent Makefile...
MODULE =

ifndef MODULE
$(error This Makefile should only be called from its parent)
endif

SRCS =  AKRL2EKB.pm \
	AKRL/AKRLList.pm \
	AKRL/AKRLTerm.pm \
	DRUM/DRUMAASite.pm \
	DRUM/DRUMMatch.pm \
	DRUM/DRUMMutation.pm \
	DRUM/DRUMTerm.pm \
	EKBAgent.pm \
	EKB/Compare.pm \
	EKB/Match.pm \
	EKB/Reasoner/Drum.pm \
	EKB/Reasoner/IndexCards.pm \
	EKB/Reasoner/CWMS.pm \
	EKB/Reasoner/Rule.pm \
	EKB/Reasoner.pm \
	EKB/Store.pm \
	EKB/Test.pm \
	EKB.pm \
	Ont/BioEntities.pm \
	Ont/BioEvents.pm \
	Ont/Geo.pm \
	Ont/GenericTerms.pm \
	Ont/Ontology.pm \
	StringParser.pm

SRCS_IN = \
	Ont/TRIPS.pm.in

SRCS_OUT = $(SRCS_IN:.in=)

SRCS += $(SRCS_OUT)

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

$(SRCS_OUT): %: %.in
	../tools/make-ont-file.pl $< > $@
