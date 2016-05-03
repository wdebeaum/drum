#
# config/ruby/lib.mk
#
# George Ferguson, ferguson@cs.rochester.edu, 27 Jul 2005
# Time-stamp: <Fri May  4 08:56:37 EDT 2007 ferguson>
#
# The following should be defined before this file is included:
#  MODULE - The name of this TRIPS module
#  SRCS - The Ruby source files to install
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk

all default::

clean::

install::
	$(MKINSTALLDIRS) $(etcdir)/$(MODULE)
	$(INSTALL_DATA) $(SRCS) $(etcdir)/$(MODULE)
