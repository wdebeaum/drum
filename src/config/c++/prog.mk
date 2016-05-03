#
# cprog.mk : Build a C program for the TRIPS System
#
# George Ferguson, ferguson@cs.rochester.edu, 9 Jan 1997
# $Id: prog.mk,v 1.1.1.1 2005/01/14 19:47:58 ferguson Exp $
#
# The following should be defined before this file is included:
#  MODULE - The name for this component, ie., its TRIPS directory name
#  PROG - The executable to build
#  SRCS - The source files to compile
#  HDRS - Any header files that are part of the package
#  MANS - Any manpages
#  XTRA - Anything else that should be checked-in with SRCS, HDRS, and MANS
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/c++/defs.mk

OBJS = $(SRCS:.cpp=.o)

all:: $(PROG)

$(PROG): $(OBJS) $(EXTRA_DEPS)
	$(CXX) $(LDFLAGS) $(EXTRA_LDFLAGS) -o $@ $(OBJS) $(LIBS)

# Make objects from C sources with per-file defs allowed
.cpp.o:
	$(CXX) -c $(CXXFLAGS) $(EXTRA_CXXFLAGS_$<) $<

install:: $(PROG)
	$(MKINSTALLDIRS) $(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(bindir)

install.man::
	$(MKINSTALLDIRS) $(mandir)/man1
	@for m in $(MANS); do \
	  $(INSTALL_DATA) $$m $(mandir)/man1/`basename $$m .man`.1; \
	done

clean::
	$(RM) $(PROG) $(OBJS)
