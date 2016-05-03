#
# File: src/Systems/Makefile
# Creator: George Ferguson
# Created: Mon Jul 16 13:00:38 2007
# Time-stamp: <Wed Oct 23 00:25:43 CDT 2013 lgalescu>
#

SUBDIRS = plow plot cardiac savant STEP sense joust kitchen obtw asma \
	pim-taf gps speech-demo woz

default:
	@for d in $(SUBDIRS); do \
	  if test -d $$d; then \
	    (cd $$d; $(MAKE)); \
	  fi; \
	done

all install clean:
	@for d in $(SUBDIRS); do \
	  if test -d $$d; then \
	    (cd $$d; $(MAKE) $@); \
	  fi; \
	done
