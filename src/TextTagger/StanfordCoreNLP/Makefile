# Makefile for installing CoreNLPFilter in TRIPS

MAIN	= CoreNLPFilter
PACKAGE	= TRIPS.TextTagger.StanfordCoreNLP
SRCS	= CoreNLPFilter.java
CLASSES = $(SRCS:.java=.class)

CONFIGDIR=../../config
include $(CONFIGDIR)/java/prog.mk
include $(CONFIGDIR)/TextTagger/defs.mk

ifeq (,$(TEXTTAGGER_corenlp))
$(error Stanford CoreNLP directory not configured)
endif

ifeq (,$(CORENLP_JAR))
$(error can\'t find Stanford CoreNLP jar file)
endif

ifeq (,$(CORENLP_MODELS_JAR))
$(error can\'t find Stanford CoreNLP models jar file)
endif

EXTRA_CLASSPATH		= $(CORENLP_CLASSPATH)
JFLAGS			= -cp $(EXTRA_CLASSPATH)
EXTRA_JAVA_FLAGS	= $(CORENLP_JAVA_FLAGS)

