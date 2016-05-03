#
# File: src/config/Graphviz/configure.ac
# Creator: William de Beaumont
# Created: 2015-12-09
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-Graphviz, 2015-12-09, wdebeaum@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

AC_CANONICAL_TARGET
AC_SUBST(TARGET, "$target")

ac_packages='dot_bin_dir dot_lib_dir'

ac_dot_bin_dir_search_list='/u/www/research/cisd/projects/trips/parser/graphviz/bin /usr/bin /usr/local/bin /bin'
AC_ARG_WITH(dot_bin_dir,
	    AC_HELP_STRING([--with-dot-bin-dir],
	    		   [locate binary dir where Graphviz dot is installed]))
AC_SUBST(DOT_BIN_DIR)
ac_dot_bin_dir_search_file='dot'

ac_dot_lib_dir_search_list='/u/www/research/cisd/projects/trips/parser/graphviz/lib64 /usr/lib64 /usr/lib /usr/local/lib64 /usr/local/lib /lib64 /lib'
AC_ARG_WITH(dot_lib_dir,
	    AC_HELP_STRING([--with-dot-lib-dir],
	    		   [locate library dir where Graphviz libraries are installed]))
AC_SUBST(DOT_LIB_DIR)
ac_dot_lib_dir_search_file='graphviz'

for pkg in $ac_packages; do
    eval "ac_with_pkg=\"\$with_${pkg}\""
    eval "ac_pkg_var=`echo \"\$pkg\" |tr '[a-z]' '[A-Z]'`"
    if test "$ac_with_pkg" = no ; then
	eval "$ac_pkg_var=''"
    else
	AC_MSG_CHECKING([for $pkg])
	ac_pkg_found=no
	eval "ac_pkg_search_file=\"\$ac_${pkg}_search_file\""
	if test "$ac_with_pkg"; then
	    ac_pkg_search_list="$ac_with_pkg"
	else
	    eval "ac_pkg_search_list=\"\$ac_${pkg}_search_list\""
	fi
	for x in $ac_pkg_search_list; do
	    if test -e "$x/$ac_pkg_search_file"; then
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
