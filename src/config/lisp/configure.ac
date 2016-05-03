#
# configure.ac for lisp
#
# George Ferguson, ferguson@cs.rochester.edu, 31 May 2002
# $Id: configure.ac,v 1.7 2010/01/20 17:18:54 ferguson Exp $
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-lisp, 2002-05-31, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

# lisp
AC_ARG_WITH(lisp, AC_HELP_STRING([--with-lisp],
		  		 [specify Common Lisp system (default "lisp")]),
		  AC_SUBST(LISP, $withval),
		  AC_CHECK_PROGS(LISP, lisp, lisp))

if test "$with_lisp" != no; then

# fasl
AC_ARG_WITH(fasl, AC_HELP_STRING([--with-fasl],
		  		 [specify compiled Lisp suffix]),
		  AC_SUBST(FASL, $withval),
		  [
AC_MSG_CHECKING([compiled Lisp suffix])
echo '(in-package :common-lisp-user)' >conftest.lisp
echo '(compile-file "conftest") #+(or mcl cmu) (quit)' | $LISP >/dev/null 2>&1
ac_object_filename=`ls | grep -v conftest.lisp | grep conftest`
if test "$ac_object_filename"; then
    FASL=`echo "$ac_object_filename" | sed s,conftest\.,,`
    AC_MSG_RESULT($FASL)
else
    AC_MSG_ERROR([couldn't determine compiled Lisp suffix; try using --with-fasl])
fi
AC_SUBST(FASL)
])

# lisp flavor
AC_ARG_WITH(lisp-flavor, AC_HELP_STRING([--with-lisp-flavor],
		  		        [specify Lisp flavor]),
					AC_SUBST(LISP_FLAVOR, $withval),
					[
AC_MSG_CHECKING([Lisp flavor])
ac_found=0
for f in allegro ccl openmcl mcl cmu ecl sbcl abcl; do
    if echo "(find :$f *features*) #+(or mcl cmu) (quit)" | $LISP | grep -i :$f >/dev/null; then
        if test $f = cmu; then
	    f=cmucl
	fi
        AC_SUBST(LISP_FLAVOR, [$f])
	ac_found=1
	break
    fi
done
if (test $ac_found = 0); then
    AC_MSG_ERROR([couldn't determine Lisp flavor; try using --with-lisp-flavor])
fi
AC_MSG_RESULT($LISP_FLAVOR)
])

fi

# runtime lisp (different for Allegro on Windows)
AC_ARG_WITH(runtime-lisp, AC_HELP_STRING([--with-runtime-lisp],
		  		 [specify Common Lisp system to use at runtime (default same as --with-lisp)]),
		  AC_SUBST(RUNTIME_LISP, $withval),
		  AC_SUBST(RUNTIME_LISP, $LISP))

# Do substitutions in files...
AC_CONFIG_FILES([Makefile defs.mk])
AC_OUTPUT


# After creating defs.mk, go compile defsystem
if test "$with_lisp" != no; then
  AC_MSG_NOTICE([compiling defsystem...])
  (cd defsystem; make)
fi
