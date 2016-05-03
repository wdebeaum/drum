CONFIGDIR=../../config
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/java/defs.mk
include $(CONFIGDIR)/perl/defs.mk
include $(CONFIGDIR)/ruby/defs.mk
include $(CONFIGDIR)/saxon/defs.mk
include $(CONFIGDIR)/DeepSemLex/defs.mk

export TRIPS_BASE=$(prefix)

# TODO detect version numbers and pass as XSL parameters so we can include them in provenance instead of hardcoding them

RESOURCE_SUBDIRS=PropBank/frames OntoNotes/sense-inventories VerbNet WordNet

all: $(RESOURCE_SUBDIRS:%=%/COMPLETE)

clean:
	rm -rf $(RESOURCE_SUBDIRS)

$(RESOURCE_SUBDIRS):
	mkdir -p $@

ON_SIS=$(shell ls $(ONTONOTES)/data/english/metadata/sense-inventories/*.xml \
	       |$(PERL) -n -e 'print "$$1\n" if (/\/([^\/-]+-[nv])\.xml$$/);')

OntoNotes/sense-inventories/COMPLETE: $(ON_SIS:%=OntoNotes/sense-inventories/%.lisp)
	touch $@

OntoNotes/sense-inventories/%.lisp: $(ONTONOTES)/data/english/metadata/sense-inventories/%.xml ../code/converters/ontonotes-inventory-to-dsl.xsl OntoNotes/sense-inventories
	cat $< \
	| grep -v 'DOCTYPE\|SYSTEM\|\\<?xml' \
	| $(JAVA) -jar $(SAXON_JAR) \
	    -xsl:../code/converters/ontonotes-inventory-to-dsl.xsl -s:- -o:$@

PB_WORDS=$(shell ls $(ONTONOTES)/data/english/metadata/frames/*.xml \
		 |$(PERL) -n -e 'chomp; print "$$1\n" if (/\/([^\/\.]+)\.xml$$/ and -r);')

PropBank/frames/COMPLETE: $(PB_WORDS:%=PropBank/frames/%.lisp)
	touch $@

PropBank/frames/%.lisp: $(ONTONOTES)/data/english/metadata/frames/%.xml ../code/converters/prep-propbank-frameset.pl ../code/converters/propbank-frameset-to-dsl.xsl PropBank/frames
	  cat $< \
	  | $(PERL) ../code/converters/prep-propbank-frameset.pl \
	  | $(JAVA) -jar $(SAXON_JAR) \
	      -xsl:../code/converters/propbank-frameset-to-dsl.xsl -s:- -o:$@

VN_CLASSES=$(shell ls $(VERBNET)/*.xml \
		   |$(PERL) -n -e 'print "$$1\n" if (/\/([^\/]+?)\.xml$$/);')

VerbNet/COMPLETE: $(VN_CLASSES:%=VerbNet/%.lisp)
	touch $@

VerbNet/%.lisp: $(VERBNET)/%.xml ../code/converters/verbnet-to-dsl.xsl
	$(JAVA) -jar $(SAXON_JAR) \
	  -xsl:../code/converters/verbnet-to-dsl.xsl -s:$< -o:$@

WordNet/COMPLETE: ../code/converters/wnsql-to-dsl.rb WordNet
	cd WordNet ; $(RUBY) ../$<
	touch $@

# fetch FrameNet XML files from the web

FrameNet/LICENSE:
	mkdir -p $@
	echo "FrameNet Data Release 1.5 by http://framenet.icsi.berkeley.edu is licensed under a Creative Commons Attribution 3.0 Unported License https://creativecommons.org/licenses/by/3.0/" >$@/LICENSE

FrameNet/%Index.xml: FrameNet/LICENSE
	cd FrameNet ; curl -O "https://framenet2.icsi.berkeley.edu/fnReports/data/$*Index.xml"

FrameNet/%Index.urls: FrameNet/%Index.xml ../code/converters/framenet-index-to-urls.xsl
	$(JAVA) -jar $(SAXON_JAR) \
	  -xsl:../code/converters/framenet-index-to-urls.xsl -s:$< -o:$@

FrameNet/%/COMPLETE-XML: FrameNet/%Index.urls
	mkdir -p FrameNet/$*
	cd FrameNet/$* ; \
	for url in `cat ../../$<` ; do \
	  curl -O "$$url" ; \
	done
	touch $@

FrameNet/frame/%.lisp: FrameNet/frame/%.xml ../code/converters/framenet-frame-to-dsl.xsl
	$(JAVA) -jar $(SAXON_JAR) \
	  -xsl:../code/converters/framenet-frame-to-dsl.xsl -s:"$<" -o:"$@"

# keep everything
.PRECIOUS: FrameNet/LICENSE FrameNet/%Index.xml FrameNet/%Index.urls FrameNet/%/COMPLETE-XML FrameNet/frame/%.lisp

FN_FRAMES=$(shell ls FrameNet/frame/*.xml \
		  |$(PERL) -n -e 'print "$$1\n" if (/\/([^\/\.]+)\.xml$$/);')

# if we have .xml files but no .lisp files, it means we got the .xml files
# after we filled the FN_FRAMES variable, so recursively call make on this same
# Makefile to actually make the .lisp files
FrameNet/frame/COMPLETE: FrameNet/frame/COMPLETE-XML $(FN_FRAMES:%=FrameNet/frame/%.lisp)
	ls FrameNet/frame/*.lisp >/dev/null || \
	( ls FrameNet/frame/*.xml >/dev/null && \
	  for l in `ls FrameNet/frame/*.xml \
		    |$(PERL) -n -e 'print "FrameNet/frame/$$1.lisp\n" \
		                      if (/\/([^\/\.]+)\.xml$$/);'` ; \
	  do \
	    $(MAKE) $$l ; \
	  done \
	)
	touch $@

# TODO FrameNet/lu/%.lisp

