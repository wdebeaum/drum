# Makefile for EKB-related scripts
#
# Time-stamp: <Fri Dec 18 20:39:12 CST 2020 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 18 Dec 2020
#

PROGS = ekb_reasoner ekb_join

CONFIGDIR=../../config
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/perl/defs.mk

all: $(PROGS)

%: %.pl
	sed -e 's@TRIPS_BASE_DEFAULT = .*$$@TRIPS_BASE_DEFAULT="$(prefix)";@' \
	  $< >$@
	chmod a+x $@

install: $(PROGS)
	$(MKINSTALLDIRS) $(bindir)
	$(INSTALL_PROGRAM) $(PROGS) $(bindir)

clean:
	$(RM) $(PROGS)
