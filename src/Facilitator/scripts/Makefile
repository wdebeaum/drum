#
# Makefile for Facilitator-related scripts
#
# Time-stamp: <Thu May 30 17:28:10 CDT 2019 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 30 May 2019
#

PROGS = find-messages get-dialogue

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
