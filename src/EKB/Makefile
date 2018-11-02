#
# Makefile for EKB
#
# Lucian Galescu <lgalescu@ihmc.us> 2017/02/03
# $Id: Makefile,v 1.4 2018/11/01 20:27:02 lgalescu Exp $

MODULE = EKBAgent
MAIN = EKBAgent.pl
SRCS = EKBAgent.pl
PERL_FLAGS = -CSD
export PERL_FLAGS

SUBDIRS = Perl # style dtd

CONFIGDIR=../config
include $(CONFIGDIR)/perl/prog.mk

all default install::
	@for d in $(SUBDIRS); do \
	  if test -d $$d; then \
	    (cd $$d; $(MAKE) $@ MODULE=$(MODULE)); \
	  fi; \
	done

DOCDIR = doc
docs: | ${DOCDIR}
	-perldoc -ohtml Perl/EKB.pm > ${DOCDIR}/EKB.html
	-perldoc -ohtml Perl/EKBAgent.pm > ${DOCDIR}/EKBAgent.html

clean::
	@echo 'No need to make $@ in $(MODULE). '

${DOCDIR}:
	mkdir -p $@
