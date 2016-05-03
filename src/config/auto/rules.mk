#
# rules.mk for auto: Targets for autoconf/configure Makefiles
#
# George Ferguson, ferguson@cs.rochester.edu,  6 Jun 2002
# $Id: rules.mk,v 1.1.1.1 2005/01/14 19:47:58 ferguson Exp $
#

default all install::

clean::
	rm -f config.log confdefs.h

distclean:: clean
	rm -f Makefile config.cache config.status *~ \#*

confclean:: distclean
	rm -f configure

#
# Targets for automatic re-configuration
#
configure: configure.ac
	autoconf

Makefile: Makefile.in config.status
	./config.status

config.status: configure
	./config.status --recheck
