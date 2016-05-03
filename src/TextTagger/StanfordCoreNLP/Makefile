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

CORENLP_JAR = $(shell ls $(TEXTTAGGER_corenlp)stanford-corenlp-?.?.?.jar 2>/dev/null || ls $(TEXTTAGGER_corenlp)stanford-corenlp-?.?.jar 2>/dev/null || ls $(TEXTTAGGER_corenlp)stanford-corenlp-????-??-??.jar 2>/dev/null)
ifeq (,$(CORENLP_JAR))
$(error can\'t find Stanford CoreNLP jar file)
endif

MODELS_JAR = $(shell ls $(TEXTTAGGER_corenlp)stanford-corenlp-?.?.?-models.jar 2>/dev/null || ls $(TEXTTAGGER_corenlp)stanford-corenlp-?.?-models.jar 2>/dev/null || ls $(TEXTTAGGER_corenlp)stanford-corenlp-models-????-??-??.jar 2>/dev/null)
ifeq (,$(MODELS_JAR))
$(error can\'t find Stanford CoreNLP models jar file)
endif

EXTRA_CLASSPATH	= $(CORENLP_JAR)$(CP_SEP)$(MODELS_JAR)$(CP_SEP)$(TEXTTAGGER_corenlp)xom.jar$(CP_SEP)$(TEXTTAGGER_corenlp)jgrapht.jar
JFLAGS = -cp $(EXTRA_CLASSPATH)
EXTRA_JAVA_FLAGS	= -Xmx3g

