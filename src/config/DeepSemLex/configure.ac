#
# File: src/config/DeepSemLex/configure.ac
# Creator: William de Beaumont
# Created: 2013-07-16
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-DeepSemLex, 2013-07-16, wdebeaum@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

AC_CANONICAL_TARGET
AC_SUBST(TARGET, "$target")

ac_packages='ontonotes verbnet'

ac_ontonotes_search_list='/p/nl/corpora/ldc/ontonotes-release-3_0 /usr/local/share/ontonotes/ontonotes-release-3_0'
AC_ARG_WITH(ontonotes,
	    AC_HELP_STRING([--with-ontonotes],
			   [locate OntoNotes dir]))
AC_SUBST(ONTONOTES)

ac_verbnet_search_list='/usr/local/share/verbnet/verbnet-3.2b /usr/local/share/verbnet/verbnet-3.2'
AC_ARG_WITH(verbnet,
	    AC_HELP_STRING([--with-verbnet],
			   [locate VerbNet dir]))
AC_SUBST(VERBNET)

for pkg in $ac_packages; do
    eval "ac_with_pkg=\"\$with_${pkg}\""
    eval "ac_pkg_var=`echo \"\$pkg\" |tr '[a-z]' '[A-Z]'`"
    if test "$ac_with_pkg" = no ; then
	eval "$ac_pkg_var=''"
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
		eval "$ac_pkg_var='$x'"
		break
	    fi
	done
	if test "$ac_pkg_found" = no; then
	    if test -z "$ac_with_pkg"; then
		AC_MSG_RESULT([not found (use --with-$pkg if this isn't ok)])
		eval "$ac_pkg_var=''"
	    else
		AC_MSG_RESULT([not found (but assuming \"$ac_with_pkg\")])
		eval "$ac_pkg_var='$ac_with_pkg'"
	    fi
	fi
    fi
done
AC_CONFIG_FILES([Makefile defs.mk])
AC_OUTPUT
