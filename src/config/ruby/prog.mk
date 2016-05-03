#
# config/ruby/prog.mk
#
# George Ferguson, ferguson@cs.rochester.edu, 27 Jul 2005
# Time-stamp: <Wed Jul 11 11:10:42 EDT 2007 ferguson>
#
# The following should be defined before this file is included:
#  MODULE - The name of this TRIPS module
#  MAIN - The main file to run
#  SRCS - The Ruby source files to install (including MAIN)
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/ruby/defs.mk

WRAPPER = $(CONFIGDIR)/ruby/run-ruby-app.sh

# Allow override of default executable name
PROG ?= $(MODULE)

all:: $(PROG)

$(PROG):: $(WRAPPER)
	sed -e 's@TRIPS_BASE_DEFAULT=.*$$@TRIPS_BASE_DEFAULT=$(prefix)@' \
	    -e 's@RUBY=.*@RUBY=$(RUBY)@' \
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
	$(RUBY) -I.. $(MAIN) $(ARGV)
