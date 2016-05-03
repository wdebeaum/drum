#
# config/perl/prog.mk
#
# George Ferguson, ferguson@cs.rochester.edu, 27 Jul 2005
# Time-stamp: <Wed Jul 11 11:10:42 EDT 2007 ferguson>
#
# The following should be defined before this file is included:
#  MODULE - The name of this TRIPS module
#  MAIN - The main file to run
#  SRCS - The Perl source files to install (including MAIN)
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/perl/defs.mk

WRAPPER = $(CONFIGDIR)/perl/run-perl-app.sh

# Allow override of default executable name
PROG ?= $(MODULE)

all:: $(PROG)

$(PROG):: $(WRAPPER)
	sed -e 's@TRIPS_BASE_DEFAULT=.*$$@TRIPS_BASE_DEFAULT=$(prefix)@' \
	    -e 's@PERL=.*@PERL=$(PERL)@' \
	    -e 's@PERL_FLAGS=.*@PERL_FLAGS=$(PERL_FLAGS)@' \
	    -e 's@MODULE=.*@MODULE=$(MODULE)@' \
	    -e 's@MAIN=.*@MAIN=$(MAIN)@' \
	    $(WRAPPER) >$(PROG)
	chmod a+x $(PROG)


install:: $(PROG)
	$(MKINSTALLDIRS) $(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(bindir)
	$(MKINSTALLDIRS) $(etcdir)/$(MODULE)
	$(INSTALL_DATA) $(SRCS) $(etcdir)/$(MODULE)

clean::
	rm -f $(PROG)

run::
	$(PERL) -I.. $(MAIN) $(ARGV)
