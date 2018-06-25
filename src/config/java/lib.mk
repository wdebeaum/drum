#
# javalib.mk : Build a Java library for the TRIPS System
#
# George Ferguson, ferguson@cs.rochester.edu, 16 Feb 1996
# $Id: lib.mk,v 1.6 2018/06/24 16:56:55 lgalescu Exp $
#
# This should be included in your Makefile if you are building
# a library (i.e., a JAR file).  If you are making a program
# you should include prog.mk instead.
#
# The following should be defined before this file is included:
#  PACKAGE - The name for this package, ie., its TRIPS directory name
#  SRCS - The Java source files to compile
#  CLASSES - The Java class files, including inner classes
#  JFLAGS - Arguments for JAVAC
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/java/defs.mk
include $(CONFIGDIR)/java/common.mk

all:: $(PACKAGE).jar

jar:: $(PACKAGE).jar

$(PACKAGE).jar:: TRIPS-stamp $(SRCS:.java=.class)
	$(JAR) cvf ${PACKAGE_DIR}/$(PACKAGE).jar $(PACKAGE_FILES)

clean::
	rm -f $(PACKAGE).jar $(MANIFEST) $(PACKAGE_FILES) *.class

install:: $(PACKAGE).jar
	$(MKINSTALLDIRS) $(etcdir)/java
	$(INSTALL_DATA) $(PACKAGE).jar $(etcdir)/java


