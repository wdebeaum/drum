#
# config/WordFinder/configure.ac
#
# George Ferguson, ferguson@cs.rochester.edu, 18 Jul 2005
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-WordFinder, 2010-01-13, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.lisp.in])
AC_CONFIG_AUX_DIR([../auto])

AC_ARG_ENABLE(wordfinder,
	      AC_HELP_STRING([--enable-wordfinder],
			     [use WordFinder to lookup unknown words]),
	      [],
	      dnl default is to check if src/WordFinder exists
	      enable_wordfinder=check)

if test "$enable_wordfinder" = check; then
    AC_MSG_CHECKING([for src/WordFinder])
    if test -d ../../WordFinder; then
	enable_wordfinder=yes
    else
	enable_wordfinder=no
    fi
    AC_MSG_RESULT($enable_wordfinder)
fi

if test "$enable_wordfinder" != no; then
    USE_WORDFINDER=t
else
    USE_WORDFINDER=nil
fi
AC_SUBST(USE_WORDFINDER)

ac_packages='wordnet comlex cmu_names'

ac_wordnet_search_list='/p/nl/wordnet/WordNet-3.0/dict/ /usr/local/share/wordnet/WordNet-3.0/dict/'
ac_comlex_search_list='/p/nl/comlex/comlex_synt_1.1.1 /usr/local/share/comlex/comlex_synt_1.1.1'
ac_cmu_names_search_list='/p/nl/cmu-names/cmu-list-of-names /usr/local/share/cmu-names/cmu-list-of-names'

AC_ARG_WITH(wordnet,
	    AC_HELP_STRING([--with-wordnet],
			   [locate wordnet directory for WordFinder]))
AC_SUBST(WORDFINDER_wordnet,nil)
AC_ARG_WITH(comlex,
	    AC_HELP_STRING([--with-comlex],
			   [locate comlex file for WordFinder]))
AC_SUBST(WORDFINDER_comlex,nil)
AC_ARG_WITH(cmu-names,
	    AC_HELP_STRING([--with-cmu-names],
			   [locate cmu-names file for WordFinder]))
AC_SUBST(WORDFINDER_cmu_names,nil)

if test "$enable_wordfinder" != no; then

for pkg in $ac_packages; do
    eval "ac_with_pkg=\"\$with_${pkg}\""
    if test "$ac_with_pkg" = no ; then
	eval "WORDFINDER_$pkg=nil"
    else
	AC_MSG_CHECKING([for $pkg])
	ac_pkg_found=no
	if test "$ac_with_pkg"; then
	    ac_pkg_search_list="$ac_with_pkg"
	else
	    eval "ac_pkg_search_list=\"\$ac_${pkg}_search_list\""
	fi
	for x in $ac_pkg_search_list; do
	    if test -e $x; then
		AC_MSG_RESULT($x)
		ac_pkg_found=yes
		eval "WORDFINDER_$pkg='\"$x\"'"
	    fi
	done
	if test "$ac_pkg_found" = no; then
	    if test -z "$ac_with_pkg"; then
		AC_MSG_RESULT([not found (use --with-$pkg if this isn't ok)])
		eval "WORDFINDER_$pkg=nil"
	    else
		AC_MSG_RESULT([not found (but assuming \"$ac_with_pkg\")])
		eval "WORDFINDER_$pkg='\"$ac_with_pkg\"'"
	    fi
	fi
    fi
done

else
    AC_MSG_NOTICE([wordfinder is not enabled])
fi

AC_CONFIG_FILES([Makefile defs.lisp])
AC_OUTPUT
