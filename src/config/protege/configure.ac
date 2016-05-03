#
# File: src/config/protege/configure.ac
# Creator: George Ferguson
# Created: Wed Feb 10 15:02:13 2010
# Time-stamp: <Mon Apr 12 12:17:28 EDT 2010 ferguson>
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-protege, 2010-02-10, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

AC_ARG_WITH(protege, AC_HELP_STRING([--with-protege], [specify Protege root]))

PROTEGE_BASE=""

if test "$with_protege" != no; then

    AC_MSG_CHECKING([for Protege])

    ac_pkg_search_list='/Applications /p/nl/protege /usr/local/share/protege'

    if test "$with_protege"; then
        dnl Test given directory exactly
	if test -d "$with_protege"; then
	    AC_MSG_RESULT($with_protege)
	else
	    AC_MSG_RESULT([not found (but assuming \"$with_protege\")])
	fi
	PROTEGE_BASE="$with_protege"
    else
        dnl Look for last-versioned Protege directory within search list dirs
	ac_pkg_found=no
    	for dir in $ac_pkg_search_list; do
    	    pdir="`ls -d $dir/Protege_3.* 2>/dev/null | tail -n 1`"
	    if test ! -z "$pdir" -a -d "$pdir"; then
		AC_MSG_RESULT($pdir)
		PROTEGE_BASE="$pdir"
		ac_pkg_found=yes
	    fi
	done
	if test "$ac_pkg_found" = no; then
	    AC_MSG_RESULT([not found (use --with-protege if this isn't ok)])
	fi
    fi
fi

AC_SUBST(PROTEGE_BASE)
AC_CONFIG_FILES([Makefile defs.mk defs.sh])
AC_OUTPUT
