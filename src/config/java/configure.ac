#
# configure.ac for java
#
# George Ferguson, ferguson@cs.rochester.edu, 3 Jun 2002
# $Id: configure.ac,v 1.4 2008/08/29 21:33:33 wdebeaum Exp $
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-java, 2002-06-03, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

# java
AC_ARG_WITH(java, AC_HELP_STRING([--with-java],
		  		 [specify Java interpreter (default "java")]),
		  AC_SUBST(JAVA, $withval),
		  AC_CHECK_PROGS(JAVA, java, java))

# javac
AC_ARG_WITH(javac, AC_HELP_STRING([--with-javac],
		  		  [specify Java compiler (default "javac")]),
		   AC_SUBST(JAVAC, $withval),
		   AC_CHECK_PROGS(JAVAC, javac, javac))

# jar
AC_ARG_WITH(jar, AC_HELP_STRING([--with-jar],
		  		[specify Java archiver (default "jar")]),
		   AC_SUBST(JAR, $withval),
		   AC_CHECK_PROGS(JAR, jar, jar))

# javadoc
AC_ARG_WITH(javadoc, AC_HELP_STRING([--with-javadoc],
		  		    [specify Java doc processor (default "javadoc")]),
		   AC_SUBST(JAVADOC, $withval),
		   AC_CHECK_PROGS(JAVADOC, javadoc, javadoc))

# default classpath separator is :, but is changed to ; for Cygwin
AC_SUBST(CP_SEP, ":")

# Java programs not in the TRIPS package don't get the "cd $prefix; " on Cygwin
AC_SUBST(NONTRIPS_JAVAC, "$JAVAC")
AC_SUBST(NONTRIPS_JAVA, "$JAVA")
AC_SUBST(NONTRIPS_JAR, "$JAR")
AC_SUBST(NONTRIPS_JAVADOC, "$JAVADOC")

# On OSX, javac has a problem with (some) symlinks
# On windows, javac doesn't handle any symlinks
need_symlink_fix_flag=no
AC_CANONICAL_TARGET
AC_SUBST(TARGET, "$target")
case "$target" in
 *-*-darwin*)
    AC_MSG_CHECKING([that OSX javac handles symlinks properly])
    ac_tmpdir=/tmp/ac$$
    mkdir -p $ac_tmpdir/src/foo
    cat <<_EOF_  >$ac_tmpdir/src/foo/Mammal.java
package TRIPS.foo;
public class Mammal {}
_EOF_
    mkdir -p $ac_tmpdir/src/bar
    cat <<_EOF_  >$ac_tmpdir/src/bar/Dog.java
package TRIPS.bar;
import TRIPS.foo.Mammal;
public class Dog extends Mammal {}
_EOF_
    ln -s .. $ac_tmpdir/src/bar/TRIPS
    if (cd $ac_tmpdir/src/bar; $JAVAC Dog.java) >/dev/null 2>&1; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
	# set a flag for this, since we handle the same way for cygwin below
	need_symlink_fix_flag=yes
    fi
    rm -rf $ac_tmpdir
    ;;
  *-*-cygwin)
    # java on windows doesn't do symlinks, even with cygwin
    # we handle this by cd'ing out to the prefix level (i.e., above src)
    # and treating things as if they were one java source tree

    # first we append a 'cd $prefix' to all of the java commands
    AC_SUBST(JAVAC, "cd $prefix; $JAVAC")
    #AC_SUBST(JAVA, "cd $prefix; $JAVA")
    AC_SUBST(JAR, "cd $prefix; $JAR")
    AC_SUBST(JAVADOC, "cd $prefix; $JAVADOC")
    AC_SUBST(CP_SEP, "\;")
    # we then move src to TRIPS below (pulled out b/c it is shared code)
    need_symlink_fix_flag=yes
    ;;
  *) AC_MSG_NOTICE([not OSX or Cygwin, assuming javac handles symlinks properly])
esac
# handle case where we do need to handle symlink problems with javac
if (test $need_symlink_fix_flag = "yes" ); then
   if test -d ../../../TRIPS; then
      AC_MSG_NOTICE([toplevel directory already adjusted])
   else
      AC_MSG_NOTICE([adjusting toplevel directory name])
      mv ../../../src ../../../TRIPS
      ln -s TRIPS ../../../src
      echo 'Renamed src to TRIPS and symlinked, because java does not handle symlinks properly on this OS' >../../../README.txt
   fi
fi

AC_CONFIG_FILES([Makefile defs.mk])
AC_OUTPUT
