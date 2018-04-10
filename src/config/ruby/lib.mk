#
# config/ruby/lib.mk
#
# George Ferguson, ferguson@cs.rochester.edu, 27 Jul 2005
# Time-stamp: <Fri May  4 08:56:37 EDT 2007 ferguson>
#
# The following should be defined before this file is included:
#  MODULE - The name of this TRIPS module/library
#  SRCS - The Ruby source files to install
# Optionally also:
#  REQUIRES - The Ruby gems required by this TRIPS module/library
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/ruby/defs.mk

all default::

clean::

install::
	$(MKINSTALLDIRS) $(etcdir)/$(MODULE)
	$(INSTALL_DATA) $(SRCS) $(etcdir)/$(MODULE)

ifdef REQUIRES

# test whether $(RUBY) can 'require' each gem in $(REQUIRES); if not, install it
install::
	for r in $(REQUIRES) ; \
	do if $(RUBY) -r$$r -e "puts '$$r found'" ; \
	   then true ; \
	   else echo "$$r not found" ; \
	        echo "sudo $(GEM) install $$r" ; \
	        sudo $(GEM) install $$r ; \
	   fi || exit 1 ; \
	done

endif
