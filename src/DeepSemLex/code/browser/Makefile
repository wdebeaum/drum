# Makefile for DSL ontology browser
# William de Beaumont
# 2014-06-11

CONFIGDIR = ../../../config
include $(CONFIGDIR)/defs.mk
TRIPS_BASE = $(prefix)
DATA_BUILD_DIR = ../../data/browser
WWW_DIR = $(prefix)/etc/ont-browser

export TRIPS_BASE

TRIPS_ONT_FILES = $(wildcard ../../../OntologyManager/Data/LFdata/*.lisp)
# too many of these, they slow down make
#TRIPS_LEX_FILES = $(wildcard ../../../LexiconManager/Data/new/*.lisp)
TRIPS_TEMPL_FILES = $(wildcard ../../../LexiconManager/Data/templates/*templates.lisp)
TRIPS_ALL_FILES = $(TRIPS_ONT_FILES) $(TRIPS_TEMPL_FILES)

all:: $(DATA_BUILD_DIR)/COMPLETE

$(DATA_BUILD_DIR)/COMPLETE: write-trips-ont-xmls.sh write-trips-ont-xmls.lisp $(TRIPS_ALL_FILES)
	mkdir -p $(DATA_BUILD_DIR)
	./write-trips-ont-xmls.sh $(DATA_BUILD_DIR)
	touch $@

install:: all ont-type.xsl exslt/str.tokenize.template.xsl
	$(MKINSTALLDIRS) $(WWW_DIR)
	$(INSTALL_DATA) \
		$(DATA_BUILD_DIR)/*.xml \
		ont-type.xsl \
		exslt/str.tokenize.template.xsl \
		$(WWW_DIR)

clean::
	rm -rf $(DATA_BUILD_DIR)

