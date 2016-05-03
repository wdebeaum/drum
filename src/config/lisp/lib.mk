#
# lib.mk for config/lisp
#
# George Ferguson, ferguson@cs.rochester.edu, 16 Feb 1996
# Time-stamp: <Mon Jul 16 13:47:28 EDT 2007 ferguson>
#
# See common.mk for description of variables that affect Lisp libraries.
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/lisp/defs.mk
include $(CONFIGDIR)/lisp/$(LISP_FLAVOR)/defs.mk
include $(CONFIGDIR)/lisp/common.mk

#
# Don't bother to install anything for a Lisp lib
#
install install.man::
	@echo 'Nothing to install'
