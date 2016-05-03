#
# javaprog.mk : Build a Java program for the TRIPS System
#
# George Ferguson, ferguson@cs.rochester.edu, 16 Feb 1999
# $Id: prog.mk,v 1.12 2012/08/08 14:53:21 ferguson Exp $
#
# This should be included in your Makefile if you are building
# a program (i.e., something that runs).  If you are making a library
# (i.e., a JAR file) you should include lib.mk instead.
#
# The following should be defined before this file is included:
#  MAIN - The main class (ie., the one with main())
#  PACKAGE - The package that the main class is in
#  USES - List of other packages that this need to find
#  SRCS - The Java source files to compile
#  CLASSES - The Java class files to include in the jar
#  XTRA - Other files to be included in the jar for this app
#  XTRA2 - More files, included without pathname translation
#
#  JFLAGS - Arguments for JAVAC
#  EXTRA_JAVA_FLAGS - Arguments for JAVA
#  EXTRA_CLASSPATH - Stuff to be added to CLASSPATH at runtime
#

include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/java/defs.mk
include $(CONFIGDIR)/java/common.mk

WRAPPER = $(CONFIGDIR)/java/run-java-app.sh

PROG = $(MAIN)

all:: $(PROG)

$(PROG):: $(PACKAGE).jar $(WRAPPER)
	sed -e 's@TRIPS_BASE_DEFAULT=.*$$@TRIPS_BASE_DEFAULT=$(prefix)@' \
	    -e 's@JAVA=.*@JAVA=$(JAVA)@' \
	    -e 's@CP_SEP=.*@CP_SEP="$(CP_SEP)"@' \
	    -e 's@TARGET=.*@TARGET="$(TARGET)"@' \
	    -e 's@PACKAGE=.*@PACKAGE=$(PACKAGE)@' \
	    -e 's@MAIN=.*@MAIN=$(MAIN)@' \
	    -e 's@USES=.*@USES="$(USES)"@' \
	    -e 's@EXTRA_JAVA_FLAGS=.*@EXTRA_JAVA_FLAGS="$(EXTRA_JAVA_FLAGS)"@' \
	    -e 's@EXTRA_CLASSPATH=.*@EXTRA_CLASSPATH="$(EXTRA_CLASSPATH)"@' \
	    -e "s@TRIPS_VERSION@$(TRIPS_VERSION)@" \
	    $(WRAPPER) >$(PROG)
	chmod a+x $(PROG)

install:: $(PROG) jar
	$(MKINSTALLDIRS) $(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(bindir)
	$(MKINSTALLDIRS) $(etcdir)/java
	$(INSTALL_DATA) $(PACKAGE).jar $(etcdir)/java

jar:: $(PACKAGE).jar

MANIFEST = MANIFEST.MF
$(MANIFEST): Makefile
	echo "Main-Class: $(PACKAGE).$(MAIN)" >$(MANIFEST)

$(PACKAGE).jar:: TRIPS-stamp $(SRCS:.java=.class) $(MANIFEST)
	$(JAR) cvfm ${PACKAGE_DIR}/$(PACKAGE).jar ${PACKAGE_DIR}/$(MANIFEST) $(PACKAGE_FILES) $(XTRA2)

clean::
	rm -f $(PROG) $(PACKAGE).jar $(MANIFEST) $(CLASS_FILES)

run::
	$(JAVA) $(EXTRA_JAVA_FLAGS) $(PACKAGE).$(MAIN) $(ARGV)

test::
	$(JAVA) $(EXTRA_JAVA_FLAGS) $(PACKAGE).$(MAIN) -connect nil $(ARGV)
