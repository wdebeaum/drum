#
# config/mozilla/configure.ac
#
# Creator: George Ferguson
# Created: Mon Feb 19 13:45:34 2007
# Time-stamp: <Thu Dec  4 17:32:33 CST 2008 lgalescu>
#
# Process this file with autoconf to produce a configure script.
#

AC_INIT(TRIPS-config-speech-out, 2007-14-19, ferguson@cs.rochester.edu)
AC_CONFIG_SRCDIR([defs.mk.in])
AC_CONFIG_AUX_DIR([../auto])

AC_ARG_ENABLE(speech-out,
	      AC_HELP_STRING([--with-speech-out],
			     [Which speech-out component to use: festival|mac]))

if test "$with_speech_out" != no; then
    if test ! -z "$with_speech_out"; then
	SPEECH_OUT_COMPONENT="$with_speech_out"
    else
	# Default varies by platform
	AC_CANONICAL_HOST
	case $host in
            *-*-darwin*)
		SPEECH_OUT_COMPONENT=SpeechOutMac
		;;
	    *)
		# SPEECH_OUT_COMPONENT=SpeechOutFestival
		# lgalescu, 12/04/2008: 
		# use --with-speech-out=festival if you have festival installed
		SPEECH_OUT_COMPONENT=SpeechOutNot
		;;
	esac
    fi
else
    # If no speech-out, need the surrogate
    SPEECH_OUT_COMPONENT=SpeechOutNot
fi

AC_MSG_NOTICE([using speech-out component: $SPEECH_OUT_COMPONENT])

AC_SUBST(SPEECH_OUT_COMPONENT)
AC_CONFIG_FILES([Makefile defs.mk])
AC_OUTPUT
