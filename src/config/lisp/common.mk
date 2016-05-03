#
# common.mk for config.lisp
#
# George Ferguson, ferguson@cs.rochester.edu, 12 Jan 2004
# Time-stamp: <Mon Mar 10 15:19:28 EDT 2008 ferguson>
#
# Functions used in these targets are defined in the lisp-specific
# directory's defs.mk.
#
# The following variables are used:
#  NAME          - Name of the module
#  DEFAULT       - Default target for make (typically compile or dump)
#  COMPILE_STYLE - Either compile-system or compile-nothing
#  IMAGE         - Filename for dumped lisp image
#  TOPLEVEL      - Toplevel function when lisp restarts
#  DUMP_STYLE    - Either dump-system or dump-image
# For dumping using dump-system:
#  DEFSYS        - File containing defsys
#  SYSTEM        - System defined therein
# For dumping using dump-image:
#  LOADFILE      - File to load everything
# 

#
# Compile by default (although dumping would also be reasonable, we
# generally link all the Lisp components into one image and dump that
# instead)
#
DEFAULT ?= compile

default all:: $(DEFAULT)

#
# Compile
#

# Default for "make compile" (allow override)
COMPILE_STYLE ?= compile-system

compile:: $(COMPILE_STYLE)

compile-system:
	$(call compile-system,$(DEFSYS),$(SYSTEM))

compile-nothing:
	@echo 'No need to compile'

#
# Dump
#

# Default for "make dump" (allow override)
DUMP_STYLE ?= dump-system

dump:: $(IMAGE)

# We don't have explicit dependencies for lisp files in a form that
# make can use. So instead of unconditional dumping (or not dumping) like
# I used to do, I'm trying this find/perl incantation. It will cause the
# image to be dumped if any fasl is newer than the current core file.
# Maybe that will be close enough for government work.
latest_fasl := $(shell find $(prefix)/src -name '*.$(FASL)' | perl -nl -e '$$mtime=(stat $$_)[9]; if ($$mtime > $$newest) { $$newest=$$mtime; $$name=$$_; } END { print $$name }')

$(IMAGE):: $(latest_fasl)
	$(MAKE) $(DUMP_STYLE)

# Hmmm... would be better if we were using defcomponent
# But this setting can always be overridden if necessary
TOPLEVEL ?= $(subst :,,$(SYSTEM))::run

dump-system:
	@echo 'Dumping system $(SYSTEM)...'
	$(call dump-system,$(DEFSYS),$(SYSTEM),$(IMAGE),$(TOPLEVEL))

dump-image:
	@echo 'Dumping image $(IMAGE)...'
	$(call dump-image,$(LOADFILE),$(IMAGE),$(TOPLEVEL))

#
# Clean
#
clean::
	$(call cleanup)
