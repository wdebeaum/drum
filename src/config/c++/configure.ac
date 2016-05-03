#
# configure.ac for c++
#
# George Ferguson, ferguson@cs.rochester.edu, 16 Sep 2002
# $Id: configure.ac,v 1.1.1.1 2005/01/14 19:47:58 ferguson Exp $
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-c++, 2002-09-16, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

# Checks for programs.
AC_PROG_CXX

# Checks for libraries.
AC_CHECK_LIB(nsl, gethostbyname)
AC_CHECK_LIB(socket, connect)

# Checks for header files.
AC_HEADER_STDC

AC_CONFIG_FILES([Makefile defs.mk])
AC_OUTPUT
