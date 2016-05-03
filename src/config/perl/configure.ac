#
# configure.ac for perl
#
# George Ferguson, ferguson@cs.rochester.edu, 3 Jun 2002
# $Id: configure.ac,v 1.3 2007/10/22 16:55:29 ferguson Exp $
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-perl, 2007-10-18, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

AC_ARG_WITH(perl,
	    AC_HELP_STRING([--with-perl],
			   [specify Perl interpreter]))
if test ! -z "$with_perl"; then
    PERL="$with_perl"
    AC_MSG_CHECKING([for perl])
    if test -e "$PERL"; then
        AC_MSG_RESULT($PERL)
    else
        AC_MSG_RESULT([$PERL (not found)])
    fi
else
    AC_PATH_PROG([PERL], [perl], [/usr/bin/perl])
fi

AC_ARG_WITH(perl-lib,
	      AC_HELP_STRING([--with-perl-lib],
			     [Local perl extensions]))
if test ! -z "$with_perl_lib"; then
    PERL_LIB="${with_perl_lib}"
    AC_MSG_NOTICE([PERL_LIB = $PERL_LIB])
fi

AC_SUBST(PERL)
AC_SUBST(PERL_LIB)

AC_CONFIG_FILES([Makefile defs.mk])
AC_OUTPUT
