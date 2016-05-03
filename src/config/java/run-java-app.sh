#!/bin/sh
#
# run-java-app.sh: Shell script wrapper for launching a Java app
#
# George Ferguson, ferguson@cs.rochester.edu,  5 Mar 1998
# $Id: run-java-app.sh,v 1.7 2016/01/26 15:07:07 wdebeaum Exp $
#
# This file will be customized for a specific application by setting
# the variables at the top of the file. If this file's name is not
# "trips-java-app.sh", then you are looking at such a customization.
#

# Per-application settings
PACKAGE=TRIPS.Module
MAIN=ModuleApplication
USES="TRIPS.TripsModule TRIPS.KQML TRIPS.util"
EXTRA_JAVA_FLAGS=''
EXTRA_CLASSPATH=''
CP_SEP=''
TARGET=''

# Root of TRIPS installation
TRIPS_BASE_DEFAULT=.
if test -z "$TRIPS_BASE"; then
    TRIPS_BASE=$TRIPS_BASE_DEFAULT
    export TRIPS_BASE
fi

# Java interpreter
if test -z "$JAVA"; then
    JAVA=/usr/java/bin/java
fi

# Java archives used by this application
JARS=$TRIPS_BASE/etc/java/$PACKAGE.jar
for pkg in $USES; do
    JARS="$JARS:$TRIPS_BASE/etc/java/$pkg.jar"
done

# CLASSPATH for Java
CLASSPATH=$JARS${EXTRA_CLASSPATH:+:$EXTRA_CLASSPATH}${CLASSPATH:+:$CLASSPATH}
case "$TARGET" in
  *-*-cygwin)
    CLASSPATH=`cygpath -w -p $CLASSPATH`
esac
export CLASSPATH

# Additional properties for Java
JFLAGS="-DTRIPS.TRIPS_BASE=$TRIPS_BASE $EXTRA_JAVA_FLAGS"

if test "$1" = "classpath" ; then
  echo $CLASSPATH |perl -p -e 's/\\/\\\\/g; s/\"/\\\"/g; s/^/"/; s/$/"/;'
else
  # Run Java
  exec $JAVA $JFLAGS $PACKAGE.$MAIN ${1:+"$@"}
fi
