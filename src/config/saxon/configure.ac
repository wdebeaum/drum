#
# File: src/config/saxon/configure.ac
# Creator: William de Beaumont
# Created: 2009-01-09
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-saxon, 2009-01-09, wdebeaum@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

AC_CANONICAL_TARGET
AC_SUBST(TARGET, "$target")

ac_saxon_jar_search_list='/usr/local/share/saxon9he.jar'

AC_ARG_WITH(saxon-jar,
	    AC_HELP_STRING([--with-saxon-jar],
			   [locate Saxon jar file]))
AC_SUBST(SAXON_JAR)

if test "$with_saxon_jar" = no ; then
    SAXON_JAR=''
else
    AC_MSG_CHECKING([for saxon jar])
    ac_pkg_found=no
    if test "$with_saxon_jar"; then
	ac_saxon_jar_search_list="$with_saxon_jar"
    fi
    for x in $ac_saxon_jar_search_list; do
	if test -e $x; then
	    AC_MSG_RESULT($x)
	    ac_pkg_found=yes
	    SAXON_JAR="$x"
	fi
    done
    if test "$ac_pkg_found" = no; then
	if test -z "$with_saxon_jar"; then
	    AC_MSG_RESULT([not found (use --with-saxon-jar if this isn't ok)])
	    SAXON_JAR=''
	else
	    AC_MSG_RESULT([not found (but assuming \"$with_saxon_jar\")])
	    SAXON_JAR="$with_saxon_jar"
	fi
    fi
fi
AC_CONFIG_FILES([Makefile defs.mk])
AC_OUTPUT
