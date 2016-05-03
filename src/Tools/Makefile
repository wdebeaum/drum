#
# Makefile for TRIPS Input Manager Tools
#
# George Ferguson, ferguson@cs.rochester.edu,  7 Apr 1995
# Time-stamp: <Tue Feb 19 12:49:42 CST 2008 lgalescu>
#

MODULE = Tools

PROGS = trips_msg   trips_cat   trips_client   trips_exec
SRCS  = trips_msg.c trips_cat.c trips_client.c trips_exec.c
HDRS =
MANS = $(PROGS:%=%.man)
XTRA =

# Build everything by default
all: $(PROGS)

#
# Stuff snarfed from config/rules/cprog.mk (which only handles a single prog)
#
CONFIGDIR=../config
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/c/defs.mk

EXTRA_CFLAGS = -I..
EXTRA_LIBS = -L../trlib -ltrlib -L../KQML -lKQML -L../util -lutil

# we don't normally debug this code, but if you do, comment out this line:
CFLAGS := $(filter-out -g,$(CFLAGS))

#
# Build program from corresponding .c
#
$(PROGS):
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $@.c $(LIBS)

#
# Install all programs and manpages
#
install: $(PROGS)
	$(MKINSTALLDIRS) $(bindir)
	for p in $(PROGS); do \
	  $(INSTALL_PROGRAM) $$p $(bindir); \
	done

#
# And we have several manpages to install
#
install.man:
	test -d $(TRIPS_BASE)/man || mkdir $(TRIPS_BASE)/man
	test -d $(TRIPS_BASE)/man/man1 || mkdir $(TRIPS_BASE)/man/man1
	@set -x; for f in $(PROGS); do \
	    $(CP) $$f.man $(TRIPS_BASE)/man/man1/$$f.1; \
	done

#
# Clean only executables
#
clean:
	$(RM) $(PROGS)
	rm -fr *.dSYM

#
# Depend all sources
#
depend:
	$(MAKEDEPEND) $(SRCS)

#
# Check-in regular stuff
#
ci:
	test -d RCS || mkdir RCS
	ci -u -t-"" -m"`date`" Makefile version $(SRCS) $(HDRS) $(MANS) $(XTRA) </dev/null
