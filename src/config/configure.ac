#
# configure.ac for config
#
# George Ferguson, ferguson@cs.rochester.edu, 31 May 2002
# Time-stamp: <Wed Feb 10 16:10:45 EST 2010 ferguson>
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config, 2010-02-10, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([version.mk])
AC_CONFIG_AUX_DIR([auto])

AC_PROG_INSTALL
# If using autoconf's install-sh, relativize its path
case "$INSTALL" in
    /*) ;;
    *) INSTALL='$(CONFIGDIR)'/$INSTALL;;
esac

# Define MKINSTALLDIRS for use in Makefiles
MKINSTALLDIRS="$ac_aux_dir/mkinstalldirs"
case "$MKINSTALLDIRS" in
    /*) ;;
    *) MKINSTALLDIRS='$(CONFIGDIR)'/$MKINSTALLDIRS;;
esac
AC_SUBST(MKINSTALLDIRS)

# grab $target so we can put it in defs.mk
AC_CANONICAL_TARGET

AC_CONFIG_SUBDIRS(lisp c c++ java perl ruby festival WordFinder speech-out firefox webui TextTagger RDFMatcher protege GeoSim tomcat alchemy saxon DeepSemLex scala javascript python Graphviz)

AC_CONFIG_FILES([defs.mk Makefile])
AC_OUTPUT
