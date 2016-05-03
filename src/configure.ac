#
# configure.ac for src
#
# George Ferguson, ferguson@cs.rochester.edu,  6 Jun 2002
# $Id: configure.ac,v 1.3 2007/05/14 13:12:29 ferguson Exp $
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS, 2007-05-14, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([00README.txt])
AC_CONFIG_AUX_DIR([config/auto])

# Default prefix is to install as sibling to this (src) dir
if test -z "$prefix" -o "$prefix" = 'NONE'; then
    prefix="`cd ..; pwd`"
    AC_PREFIX_DEFAULT("$prefix")
    AC_MSG_NOTICE([using prefix=$prefix])
fi

# Sphinx3 configure complains (rightly) since we haven't yet built portaudio
if test -z "$with_portaudio"; then
    AC_MSG_NOTICE([using our portaudio=$prefix/src/portaudio])
    ac_configure_args="$ac_configure_args$ac_sep'--with-portaudio=$prefix/src/portaudio'"
fi

AC_CONFIG_SUBDIRS(config portaudio Sphinx3 LMFactory)
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
