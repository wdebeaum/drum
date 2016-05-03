#
# prog.mk for config/lisp
#
# George Ferguson, ferguson@cs.rochester.edu, 16 Feb 1996
# Time-stamp: <Sun Jul 22 16:06:30 EDT 2007 ferguson>
#
# The following variables are used:
#  NAME          - Name of the module
#  PROG          - Name of executable shell script wrapper
# See also common.mk for other variables that affect Lisp programs.
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/lisp/defs.mk
include $(CONFIGDIR)/lisp/$(LISP_FLAVOR)/defs.mk
include $(CONFIGDIR)/lisp/common.mk

#
# After dumping image, need to create executable wrapper also
#
PROG ?= $(NAME)

dump:: $(PROG)

#
# Install program as script and image
#
install:: $(IMAGE) $(PROG)
	# Executable script
	$(MKINSTALLDIRS) $(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(bindir)
	# Lisp image
	$(MKINSTALLDIRS) $(etcdir)
	$(INSTALL_DATA) $(IMAGE) $(etcdir)

install.man::
	$(MKINSTALLDIRS) $(mandir)
	test ! -f $(PROG).man || $(INSTALL_DATA) $(PROG).man $(mandir)/$(PROG).1
#
# Script to launch lisp with image file
#
RUN_IMAGE_CODE = $(CONFIGDIR)/lisp/$(LISP_FLAVOR)/run-image.sh

prog $(PROG): $(MAKEFILE) $(RUN_IMAGE_CODE)
	sed -e 's@TRIPS_BASE=.*$$@TRIPS_BASE=$(prefix)@' \
	    -e 's@IMAGE=.*@IMAGE=\$$TRIPS_BASE/etc/$(IMAGE)@' \
	    -e 's@TARGET=.*@TARGET=$(target)@' \
	    -e 's@LISP=.*@LISP="$(RUNTIME_LISP)"@' \
	    -e 's@<FILENAME>@$(PROG)@' \
	    -e 's@<MODULE>@$(NAME)@' \
	    $(RUN_IMAGE_CODE) >$(PROG)
	chmod a+x $(PROG)

#
# Install program as script and image
#
install_prog: $(IMAGE) $(PROG)
	# Executable script
	$(MKINSTALLDIRS) $(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(bindir)
	# Lisp image
	$(MKINSTALLDIRS) $(etcdir)
	$(INSTALL_DATA) $(IMAGE) $(etcdir)

#
# Clean
#
clean::
	rm -f $(PROG)
