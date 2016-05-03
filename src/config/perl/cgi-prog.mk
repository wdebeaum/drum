#
# config/perl/cgi-prog.mk
#
# The following should be defined before this file is included:
#  MODULE - The name of this TRIPS module
#  MAIN - The main file to run
#  SRCS - The Perl source files to install (including MAIN)
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/perl/defs.mk

WRAPPER = $(CONFIGDIR)/perl/run-perl-cgi.pl

# Allow override of default executable name
PROG ?= $(MODULE)

all:: $(PROG)

$(PROG):: $(WRAPPER)
	sed -e 's@\$$TRIPS_BASE@$(prefix)@' \
	    -e 's@\$$PERL@$(PERL)@' \
	    -e 's@\$$PERL_FLAGS@$(PERL_FLAGS)@' \
	    -e 's@\$$MODULE@$(MODULE)@' \
	    -e 's@\$$MAIN@$(MAIN)@' \
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
