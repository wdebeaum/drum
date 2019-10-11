#
# Makefile for TextTagger
#
# William de Beaumont <wbeaumont@ihmc.us> 2008/3/6
# $Id: Makefile,v 1.124 2019/10/10 16:42:39 wdebeaum Exp $
#

MODULE = TextTagger
MAIN   = TextTagger.pl
SRCS   = TextTagger.pl
# make @ARGV and all filehandles UTF-8 by default in run script
export PERL_FLAGS = -CASD
# do the same for perl code called from this Makefile
export PERL_UNICODE = ASD
SHELL = /bin/bash

SUBDIRS = Perl StanfordNER StanfordPOSTagger StanfordParser StanfordCoreNLP ExtraModels terms2

CONFIGDIR=../config
include $(CONFIGDIR)/perl/prog.mk
include $(CONFIGDIR)/TextTagger/defs.mk
export TEXTTAGGER_umls TEXTTAGGER_semgroups TEXTTAGGER_metamap TEXTTAGGER_cj_parser TEXTTAGGER_biomodel TEXTTAGGER_corenlp TEXTTAGGER_enju TEXTTAGGER_aspell

# functions for downloading data files automatically (mostly in DRUM system)
download=mkdir -p downloads ; cd downloads ; curl -O -L $(1)
download_compressed=mkdir -p downloads ; cd downloads ; curl --compressed -O -L $(1)
download_sourceforge=mkdir -p downloads ; ( cd downloads ; curl --compressed -O -L $(1)"?format=raw" ) ; mv $@\?format=raw $@
download_useragent=mkdir -p downloads ; cd downloads ; curl --user-agent 'TextTaggerMakefile/5.0' --compressed -O -L $(1)
download_insecure=mkdir -p downloads ; cd downloads ; curl --insecure --compressed -O -L $(1)

all default install clean:: 
	@for d in $(SUBDIRS); do \
	  if test -d $$d; then \
	    (cd $$d && $(MAKE) $@ MODULE=$(MODULE)) || exit 1; \
	  fi; \
	done

distclean:: clean
	rm -rf downloads

# if this is a CVS checkout, we can get new .pl files from CVS
# (saves me telling everyone to do this manually each time I add one)
# if it's a git checkout, it should already be handled by git
ifneq (,$(wildcard CVS))
%.pl:
	cvs update $@
endif

# This makefile creates important resources; if anything fails we prevent it
# from creating empty or partial files which could negatively affect the
# system's behavior
.DELETE_ON_ERROR:

# include any separate Makefiles for optional parts of TextTagger
ifneq (,$(wildcard *.mk))
include *.mk
endif

