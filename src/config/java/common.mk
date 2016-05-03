#
# common.mk for config/java
#
# George Ferguson, ferguson@cs.rochester.edu, 22 Jan 2004
# Time-stamp: <Thu Apr  3 13:31:44 EDT 2008 ferguson>
#
# Code common to building and installing Java libraries and programs.
#
# Note special care needed when dealing with class filenames that may include
# dollar signs (`$'), such as when inner or anonymous classes are used.
# Damn Sun for picking that particular character for this...
#

# Class files with PACKAGE_DIR added and dollar-signs escaped
CLASS_FILES = $(foreach file,$(CLASSES),$(PACKAGE_DIR)/$(subst $$,\$$,$(file)))
XTRA_FILES = $(foreach file,$(XTRA),$(PACKAGE_DIR)/$(file))
PACKAGE_FILES = $(CLASS_FILES) $(XTRA_FILES)

# This is the PACKAGE as a directory with slashes.
#  For example, is TRIPS.Foo is the value of PACKAGE this will be TRIPS/Foo
PACKAGE_DIR = $(subst .,/,$(PACKAGE))

# Basic target for compiling java files
.SUFFIXES: .java .class
.java.class:
	$(JAVAC) $(JFLAGS) ${PACKAGE_DIR}/$<

# Default target if this file gets included first
all::

# Setup symlink for package name starting with `TRIPS.'
#   Need to have one `../' for each `.' in PACKAGE
# To avoid problems with the date of the linked directory being compared to
# the Makefile, we use a secondary (*.stamp) file as the make target

empty =
space = $(empty) $(empty)
package2dir = $(subst $(space),$(empty),$(patsubst %,../,$(wordlist 2,999,$(subst .,$(space),$1))))

TRIPS-stamp:: Makefile
	rm -f TRIPS
	ln -s $(call package2dir,$(PACKAGE)) TRIPS
	date >TRIPS-stamp

clean::
	rm -f TRIPS-stamp

# Javadoc
doc:: doc/index.html

doc/index.html: $(SRCS)
	test -d doc || mkdir doc
	$(JAVADOC) -d doc -private $(PACKAGE)
