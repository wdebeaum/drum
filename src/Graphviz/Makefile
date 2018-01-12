#
# Makefile for Graphviz
#
# William de Beaumont <wbeaumont@ihmc.us> 2008/3/6
# $Id: Makefile,v 1.6 2018/01/11 20:55:58 wdebeaum Exp $
#

MODULE = Graphviz
MAIN   = Graphviz.pl
SRCS   = $(MAIN) Configuration.pm Errors.pm Escape.pm WindowManager.pm
# make all filehandles UTF-8 by default
PERL_FLAGS = -CSD

all:: fix-permissions Configuration.pm

install:: Configuration.pm

CONFIGDIR=../config
include $(CONFIGDIR)/perl/prog.mk
include $(CONFIGDIR)/Graphviz/defs.mk

fix-permissions:
	chmod a+x *.rb *.sh moebius-to-dot.pl

Configuration.pm: Configuration.pm.in
	sed -e 's@"DOT_BIN_DIR"@"$(DOT_BIN_DIR)"@' \
	    -e 's@"DOT_LIB_DIR"@"$(DOT_LIB_DIR)"@' \
	    $< >$@

