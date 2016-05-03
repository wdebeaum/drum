#
# configure.ac for ruby
#
# George Ferguson, ferguson@cs.rochester.edu, 3 Jun 2002
# $Id: configure.ac,v 1.1 2008/10/14 19:48:18 wdebeaum Exp $
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-ruby, 2007-10-18, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

AC_ARG_WITH(ruby,
	    AC_HELP_STRING([--with-ruby],
			   [specify Ruby interpreter]))
if test ! -z "$with_ruby"; then
    RUBY="$with_ruby"
    AC_MSG_CHECKING([for ruby])
    if test -e "$RUBY"; then
        AC_MSG_RESULT($RUBY)
    else
        AC_MSG_RESULT([$RUBY (not found)])
    fi
else
    AC_PATH_PROG([RUBY], [ruby], [/usr/bin/ruby])
fi

AC_ARG_WITH(ruby-lib,
	      AC_HELP_STRING([--with-ruby-lib],
			     [Local ruby extensions]))
if test ! -z "$with_ruby_lib"; then
    RUBY_LIB="${with_ruby_lib}"
    AC_MSG_NOTICE([RUBY_LIB = $RUBY_LIB])
fi

AC_SUBST(RUBY)
AC_SUBST(RUBY_LIB)

AC_CONFIG_FILES([Makefile defs.mk])
AC_OUTPUT
