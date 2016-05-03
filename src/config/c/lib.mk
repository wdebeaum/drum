#
# lib.mk for c: Build a C library for the TRIPS System
#
# George Ferguson, ferguson@cs.rochester.edu, 16 Feb 1996
# $Id: lib.mk,v 1.1.1.1 2005/01/14 19:47:58 ferguson Exp $
#
# The following should be defined before this file is included:
#  MODULE - The name for this component, ie., its TRIPS directory name
#  LIB  - The library to build (without ".a" suffix)
#  SRCS - The source files to compile
#  HDRS - Any header files that are part of the package
#  MANS - Any manpages
#  XTRA - Anything else that should be checked-in with SRCS, HDRS, and MANS
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/c/defs.mk

OBJS = $(SRCS:.c=.o)

all:: $(LIB).a

$(LIB).a: $(OBJS) $(EXTRA_DEPS)
	$(AR) rv $@ $(OBJS)
	$(RANLIB) $@

# Make objects from C sources with per-file defs allowed
.c.o:
	$(CC) -c $(CFLAGS) $(EXTRA_DEFS_$<) $<

install:: $(LIB).a

install.man::
	$(MKINSTALLDIRS) $(mandir)/man3
	@for m in $(MANS); do \
	  $(INSTALL_DATA) $$m $(mandir)/man3/`basename $$m .man`.3; \
	done

clean::
	$(RM) $(LIB).a $(OBJS)

