#
# configure.ac for festival
#
# George Ferguson, ferguson@cs.rochester.edu,  12 Dec 2003
# $Id: configure.ac,v 1.1.1.1 2005/01/14 19:47:58 ferguson Exp $
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-festival, 2004-04-06, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

# Festival uses C++
AC_PROG_CXX

# Festival
FESTIVAL_BASE=""
FESTIVAL_CXXFLAGS="-fno-implicit-templates -Wno-non-template-friend -Wno-deprecated"
FESTIVAL_LIBS="-lFestival -lestools -lestbase -leststring -ltermcap"

AC_ARG_WITH(truetalk, AC_HELP_STRING([--with-festival], [specify Festival root]))

if test "$with_festival" != no; then

AC_LANG(C++)

AC_MSG_CHECKING([for Festival])
ac_saved_CXXFLAGS="$CXXFLAGS"; CXXFLAGS="$FESTIVAL_CXXFLAGS $CXXFLAGS"
ac_saved_LIBS="$LIBS"; LIBS="$FESTIVAL_LIBS $LIBS"
AC_TRY_LINK([#include <festival.h>], [festival_initialize(1, 210000);],
  [
    AC_MSG_RESULT([ok])
  ],
  [
    ac_festival_found=no
    for d in ${with_festival:+$with_festival} /usr /usr/local /usr/speech/festival/latest /p/speech/festival/latest; do
      FESTIVAL_BASE="$d"
      FESTIVAL_CXXFLAGS="-I$FESTIVAL_BASE/festival/src/include -I$FESTIVAL_BASE/speech_tools/include -fno-implicit-templates -Wno-non-template-friend -Wno-deprecated"
      FESTIVAL_LIBS="-L$FESTIVAL_BASE/festival/src/lib -lFestival -L$FESTIVAL_BASE/speech_tools/lib -lestools -lestbase -leststring -ltermcap"
      CXXFLAGS="$FESTIVAL_CXXFLAGS $CXXFLAGS"
      LIBS="$FESTIVAL_LIBS $LIBS"
      AC_TRY_LINK([#include <festival.h>], [festival_initialize(1, 210000);],
        [
	  AC_MSG_RESULT($d)
	  ac_festival_found=yes
	  break
	]
      )
    done
    if test "$ac_festival_found" = no; then
        AC_MSG_RESULT([not found (use --with-festival if this isn't ok)])
    fi
  ]
)
AC_SUBST(FESTIVAL_BASE)
AC_SUBST(FESTIVAL_CXXFLAGS)
AC_SUBST(FESTIVAL_LIBS)
CFLAGS="$ac_saved_CFLAGS"
LIBS="$ac_saved_LIBS"

fi

AC_CONFIG_FILES([Makefile defs.mk])
AC_OUTPUT
