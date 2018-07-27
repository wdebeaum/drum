#
# Makefile for TextTagger
#
# William de Beaumont <wbeaumont@ihmc.us> 2008/3/6
# $Id: Makefile,v 1.113 2018/07/26 17:45:11 wdebeaum Exp $
#

MODULE = TextTagger
MAIN   = TextTagger.pl
SRCS   = TextTagger.pl
# make all filehandles UTF-8 by default
export PERL_FLAGS = -CSD
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

#
# Generic rules for all TextTagger installations
#

all default install clean:: 
	@for d in $(SUBDIRS); do \
	  if test -d $$d; then \
	    (cd $$d && $(MAKE) $@ MODULE=$(MODULE)) || exit 1; \
	  fi; \
	done

# if this is a CVS checkout, we can get new .pl files from CVS
# (saves me telling everyone to do this manually each time I add one)
# if it's a git checkout, it should already be handled by git
ifneq (,$(wildcard CVS))
%.pl:
	cvs update $@
endif

#
# This makefile creates important resources; if anything fails we prevent it
# from creating empty or partial files which could negatively affect the
# system's behavior
#
.DELETE_ON_ERROR:

#
# Rules for those using terms.c (see also several sections using this below)
#
ifneq (,$(wildcard terms.c))
all:: terms

install:: terms
	$(INSTALL_PROGRAM) terms $(bindir)

clean::
	rm -rf terms

terms: terms.c
	gcc terms.c -o terms
endif

#
# Rules for those using the GNIS geographic names database
#
ifneq (,$(wildcard make-terms-dot-txt.sh))
all:: terms.txt

install:: terms.txt
	$(INSTALL_DATA) terms.txt $(etcdir)/$(MODULE)

clean::
	rm -rf terms.txt terms.unsorted.txt

terms.txt: make-terms-dot-txt.sh
	./make-terms-dot-txt.sh $(TEXTTAGGER_geonames)
endif

ifneq (,$(wildcard get-countries-tsv.pl))
all:: countries.tsv

install:: countries.tsv
	$(INSTALL_DATA) $< $(etcdir)/$(MODULE)

clean::
	rm -rf countries.tsv

countries.tsv: get-countries-tsv.pl downloads/countries.json
	./$^ >$@

downloads/MADE:
	mkdir -p downloads
	touch $@

downloads/countries.json: downloads/MADE
	curl -L -o $@ "https://raw.githubusercontent.com/mledoze/countries/master/countries.json"
endif

# blech, some installed files are needed by get-wn-multiwords.pl
$(prefix)/etc/WordNetSQL/WordNetSQL.pm: ../WordNetSQL/Makefile ../WordNetSQL/word_net_sql.polyglot
	( cd ../WordNetSQL/ && \
	  TRIPS_BASE=$(prefix) make install-lib \
	)

$(prefix)/etc/WordNetSQL/wn.db: ../WordNetSQL/Makefile
	( cd ../WordNetSQL/ && \
	  TRIPS_BASE=$(prefix) make install-wn \
	)

$(prefix)/etc/util/add_suffix.ph: ../util/Makefile-perl ../util/add_suffix.polyglot
	( cd ../util/ && \
	  TRIPS_BASE=$(prefix) make -f Makefile-perl install \
	)

#
# Rules for those using the WordNet multiwords tagger
#
ifneq (,$(wildcard get-wn-multiwords.pl))
WORDNET_BASEPATH=$(shell grep wordnet-basepath $(CONFIGDIR)/WordFinder/defs.lisp | awk '{gsub(/"/,"",$$3); print $$3}')

ifeq (,$(WORDNET_BASEPATH))
$(error WordNet basepath not configured)
endif

all:: wn-multiwords.tsv

install:: wn-multiwords.tsv
	$(INSTALL_DATA) wn-multiwords.tsv $(etcdir)/$(MODULE)

clean::
	rm -rf wn-multiwords.tsv

wn-multiwords.tsv: get-wn-multiwords.pl $(CONFIGDIR)/WordFinder/defs.lisp $(prefix)/etc/WordNetSQL/WordNetSQL.pm $(prefix)/etc/util/add_suffix.ph ../WordFinder/wordnet/stoplist.txt
	TRIPS_BASE=$(prefix) ./get-wn-multiwords.pl $(WORDNET_BASEPATH) >wn-multiwords.tsv
endif

#
# Rules for those using the PersonalNames tagger
#
ifneq (,$(wildcard Perl/TextTagger/PersonalNames.pm))

all:: personal-names.tsv

install:: personal-names.tsv
	$(INSTALL_DATA) $< $(etcdir)/$(MODULE)

downloads/names.zip:
	$(call download_compressed,https://www.ssa.gov/oact/babynames/names.zip)

personal-names.tsv: get-personal-names.sh downloads/names.zip
	./$^ >$@

endif

#
# Rules for those using the American/British alternate spellings list
#
ifneq (,$(wildcard Perl/TextTagger/AlternateSpellings.pm))
all:: alternate-spellings.tsv

install:: alternate-spellings.tsv
	$(INSTALL_DATA) alternate-spellings.tsv $(etcdir)/$(MODULE)

alternate-spellings.tsv:
	curl -A "Mozilla/4.0" "https://wiki.ubuntu.com/EnglishTranslation/WordSubstitution?action=raw" \
	| perl -n -e "unless (\$$_ =~ /'''/ or \$$_ =~ / \\|\\| Y \\|\\| / or not \$$_ =~ /^ \\|\\| /) { chomp; s/^ \\|\\| //; s/ \\|\\|\\s*\$$//; s/ \\|\\| /\\t/g; print \"\$$_\\n\"; }" \
	| cut -f1,2 \
	>$@

distclean::
	rm -f alternate-spellings.tsv
endif

#
# Rules for Drum
#
ifneq (,$(wildcard Perl/TextTagger/Drum.pm))

all:: drum-terms.tsv drum-dsl/COMPLETE specialist.tsv drum-dbxrefs.tsv go_protmods.tsv

install:: mirna-species.tsv drum-terms.tsv drum-dsl/COMPLETE specialist.tsv drum-dbxrefs.tsv go_protmods.tsv
	$(INSTALL_DATA) mirna-species.tsv $(etcdir)/$(MODULE)
	$(INSTALL_DATA) drum-terms.tsv $(etcdir)/$(MODULE)
	$(INSTALL_DATA) specialist.tsv $(etcdir)/$(MODULE)
	$(INSTALL_DATA) drum-dbxrefs.tsv $(etcdir)/$(MODULE)
	$(INSTALL_DATA) go_protmods.tsv $(etcdir)/$(MODULE)

# This file is in CVS, so we don't really need this rule, but it's here to
# document how the file was made in the first place. The database is a lot
# larger than the part we use, and it's public domain, so it makes sense for
# this file to be in CVS.
#mirna-species.tsv:
#	curl "ftp://mirbase.org/pub/mirbase/CURRENT/miRNA.dat.gz" \
#	| gunzip -c \
#	| perl -n -e \
#	'$$a = $$1 if (/^ID   (\w+)-/i);  push @{$$h{$$a}}, $$1 if (/^DE   ([A-Z]\w* \w*) mir/i and  not grep { $$_ eq $$1 } @{$$h{$$a}});  END {  print map { $$_ . join("", map { "\t$$_" } @{$$h{$$_}}) . "\n" }  sort keys %h;  }' \
#	>$@

drum-terms.tsv: merge-terms-files.pl obo-terms.tsv hgnc-terms.tsv uniprot-terms.tsv uniprot-subcell-terms.tsv nextprot-family-terms.tsv pfam-terms.tsv mesh-scr-terms.tsv ncit-terms.tsv hlsm-terms.tsv
	./$^ >$@

drum-dbxrefs.tsv: get-uniprot-dbxrefs.pl downloads/uniprot_sprot.dat.gz uniprot-species.pl
	gunzip -c downloads/uniprot_sprot.dat.gz \
	| ./$< >$@

OBO_FILES:=$(addprefix downloads/,famplex.obo BrendaTissueOBO cellosaurus.obo chebi.obo cl-basic.obo efo.obo go.obo pr.obo unit.obo)
# psi-mi25.obo so-xp.obo

obo-terms.tsv: get-obo-terms.pl $(OBO_FILES) Makefile
	TRIPS_BASE=$(prefix) ./get-obo-terms.pl $(OBO_FILES) >$@

go_protmods.obo: go_protmods.pl downloads/go.obo
	./go_protmods.pl -o $@ downloads/go.obo

go_protmods.tsv: get-obo-terms.pl go_protmods.obo
	TRIPS_BASE=$(prefix) ./$^ >$@

hgnc-terms.tsv: get-hgnc-terms.pl downloads/protein-coding_gene.txt.gz
	gunzip -c <downloads/protein-coding_gene.txt.gz \
	| ./get-hgnc-terms.pl \
	>$@

uniprot-terms.tsv: get-uniprot-terms.pl downloads/uniprot_sprot.dat.gz uniprot-species.pl
	gunzip -c <downloads/uniprot_sprot.dat.gz \
	| ./get-uniprot-terms.pl \
	>$@

uniprot-subcell-terms.tsv: downloads/subcell.txt get-uniprot-subcell-terms.pl
	./get-uniprot-subcell-terms.pl <$< >$@

nextprot-family-terms.tsv: downloads/cv_family.txt get-nextprot-family-terms.pl
	./get-nextprot-family-terms.pl <$< >$@

pfam-terms.tsv: get-xfam-terms.pl downloads/Pfam-A.seed.gz
	gunzip -c <downloads/Pfam-A.seed.gz \
	| ./get-xfam-terms.pl \
	>$@

mesh-scr-terms.tsv: get-mesh-scr-terms.pl $(TEXTTAGGER_mesh_scr)
	if test -z "$(TEXTTAGGER_mesh_scr)" ; then echo "Error: MeSH SCR (c2015.bin.gz) not configured" ; exit 1 ; fi
	gunzip -c <$(TEXTTAGGER_mesh_scr) \
	| ./get-mesh-scr-terms.pl \
	>$@

# get the current version of NCIt, since it changes so often and they don't
# keep the old ones available (or just use the one we have if we have it
# already)
NCIT_WEBSITE=http://evs.nci.nih.gov/ftp1/NCI_Thesaurus/
NCIT_ZIPFILE=$(shell cd downloads ; ls Thesaurus_??.???.FLAT.zip 2>/dev/null || (curl -s -L $(NCIT_WEBSITE) |grep -o -e 'Thesaurus_..\....\.FLAT.zip' |head -1))
$(info NCIT_ZIPFILE=$(NCIT_ZIPFILE))

# also writes drum-dsl/NCIT/*
ncit-terms.tsv: get-ncit-terms.pl downloads/$(NCIT_ZIPFILE)
	./$^ >$@

hlsm-terms.tsv: get-hlsm-terms.pl downloads/small_molecule.csv
	./$^ >$@

specialist.tsv: get-specialist-tsv.pl downloads/LEXICON downloads/inflection.table $(prefix)/src/LexiconManager/Data/new
	TRIPS_BASE=$(prefix) ./get-specialist-tsv.pl downloads/LEXICON downloads/inflection.table >$@

# for BOB system
ifneq (,$(wildcard Perl/TextTagger/Misspellings.pm))

ifeq (,$(TEXTTAGGER_aspell))
$(error aspell not configured. Please install aspell and re-run the configure script.)
endif

all:: drum.aspell

install:: drum.aspell
	$(INSTALL_DATA) $< $(etcdir)/$(MODULE)

# NOTE: We give aspell back its own default setting for local-data-dir because
# it actually doesn't work on Debian without this; it fails to find the "en"
# language data file in that directory. (Why, Debian, why?)
specialist.aspell: downloads/inflection.table
	cut -d'|' -f1 <$< |perl -CSD -p -e 's/\W+/\n/g;' |sort |uniq \
	|$(TEXTTAGGER_aspell) \
	  --local-data-dir="`$(TEXTTAGGER_aspell) config local-data-dir`" \
	  --encoding=utf-8 --lang=en create master ./$@ \
	2>specialist.aspell.err

drum.aspell: downloads/inflection.table drum-terms.tsv
	( cut -d'|' -f1 <downloads/inflection.table && \
	  cut -f1 <drum-terms.tsv \
	) | perl -CSD -p -e 's/\W+/\n/g;' |sort |uniq \
	| perl -n -e 'print unless (length($$_) > 100);' \
	| $(TEXTTAGGER_aspell) \
	  --local-data-dir="`$(TEXTTAGGER_aspell) config local-data-dir`" \
	  --encoding=utf-8 --lang=en create master ./$@ \
	2>drum.aspell.err

clean::
	rm -rf specialist.aspell specialist.aspell.err
	rm -rf drum.aspell drum.aspell.err

endif

downloads/famplex.obo:
	$(call download_compressed,"https://raw.githubusercontent.com/sorgerlab/famplex/master/export/famplex.obo")

# see http://www.obofoundry.org/ for up-to-date links

downloads/BrendaTissueOBO:
	$(call download_useragent,"http://www.brenda-enzymes.info/ontology/tissue/tree/update/update_files/BrendaTissueOBO")

downloads/chebi.obo:
	$(call download,"ftp://ftp.ebi.ac.uk/pub/databases/chebi/ontology/chebi.obo")

downloads/cl-basic.obo:
	$(call download_compressed,"https://raw.githubusercontent.com/obophenotype/cell-ontology/master/cl-basic.obo")

downloads/go.obo:
	$(call download_compressed,"http://purl.obolibrary.org/obo/go.obo")

downloads/cellosaurus.obo:
	# old link: ftp://ftp.nextprot.org/pub/current_release/controlled_vocabularies/cellosaurus.obo
	$(call download_compressed,"ftp://ftp.expasy.org/databases/cellosaurus/cellosaurus.obo")
	# fix escaping problem, and properly capitalize [Term]
	$(PERL) -p -i -e 's/"HeLa "Kyoto""/"HeLa \\"Kyoto\\""/; s/^\[term\]/[Term]/;' $@

downloads/efo.obo:
	$(call download_sourceforge,"http://sourceforge.net/p/efo/code/HEAD/tree/trunk/src/efoinobo/efo.obo")

downloads/pr.obo:
	$(call download_compressed,"http://purl.obolibrary.org/obo/pr.obo")

downloads/unit.obo:
	$(call download_compressed,"https://raw.githubusercontent.com/bio-ontology-research-group/unit-ontology/master/unit.obo")

downloads/so-xp.obo:
	$(call download_sourceforge,"https://sourceforge.net/p/song/svn/HEAD/tree/trunk/so-xp.obo")

downloads/psi-mi25.obo:
	$(call download,"http://psidev.cvs.sourceforge.net/viewvc/*checkout*/psidev/psi/mi/rel25/data/psi-mi25.obo")

# see http://www.genenames.org/

downloads/protein-coding_gene.txt.gz:
	$(call download,"ftp://ftp.ebi.ac.uk/pub/databases/genenames/locus_groups/protein-coding_gene.txt.gz")

# see http://www.uniprot.org/

downloads/uniprot_sprot.dat.gz:
	$(call download,"ftp://ftp.uniprot.org/pub/databases/uniprot/knowledgebase/uniprot_sprot.dat.gz")

downloads/subcell.txt:
	$(call download,"ftp://ftp.uniprot.org/pub/databases/uniprot/knowledgebase/docs/subcell.txt")

# see http://www.nextprot.org/

downloads/cv_family.txt:
	$(call download_compressed,"ftp://ftp.nextprot.org/pub/current_release/controlled_vocabularies/cv_family.txt")

# see http://xfam.org/

downloads/Pfam-A.seed.gz:
	$(call download,"ftp://ftp.ebi.ac.uk/pub/databases/Pfam/current_release/Pfam-A.seed.gz")

# NCI Thesaurus
# see http://evs.nci.nih.gov/ftp1/NCI_Thesaurus/ReadMe.txt
downloads/$(NCIT_ZIPFILE):
	$(call download,$(NCIT_WEBSITE)$(NCIT_ZIPFILE))

# HMS LINCS Small Molecule database
# see http://lincs.hms.harvard.edu/db/sm/
downloads/small_molecule.csv:
	mkdir -p downloads
	curl --compressed -L "http://lincs.hms.harvard.edu/db/sm/?search=&output_type=.csv" -o $@

# see http://lexsrv3.nlm.nih.gov/Specialist/Home/index.html

downloads/LEXICON:
	$(call download_compressed,"http://lexsrv3.nlm.nih.gov/LexSysGroup/Projects/lexicon/2015/release/LEX/LEXICON")

downloads/inflection.table:
	$(call download_compressed,"http://lexsrv3.nlm.nih.gov/LexSysGroup/Projects/lexicon/2015/release/LEX/MISC/inflection.table")

drum-dsl/FPLX/COMPLETE: downloads/famplex.obo obo-to-dsl.pl
	./obo-to-dsl.pl <$<
	touch $@

drum-dsl/BTO/COMPLETE: downloads/BrendaTissueOBO obo-to-dsl.pl
	./obo-to-dsl.pl <$<
	touch $@

drum-dsl/CHEBI/COMPLETE: downloads/chebi.obo obo-to-dsl.pl
	./obo-to-dsl.pl <$<
	touch $@

drum-dsl/CO/COMPLETE: downloads/cl-basic.obo obo-to-dsl.pl
	./obo-to-dsl.pl <$<
	touch $@

drum-dsl/EFO/COMPLETE: downloads/efo.obo obo-to-dsl.pl
	./obo-to-dsl.pl EFO Orphanet , UO <$<
	touch $@

drum-dsl/GO/COMPLETE: downloads/go.obo obo-to-dsl.pl
	./obo-to-dsl.pl <$<
	touch $@

drum-dsl/MI/COMPLETE: downloads/psi-mi25.obo obo-to-dsl.pl
	./obo-to-dsl.pl <$<
	touch $@

drum-dsl/PR/COMPLETE: downloads/pr.obo obo-to-dsl.pl
	./obo-to-dsl.pl PR <$<
	touch $@

drum-dsl/SO/COMPLETE: downloads/so-xp.obo obo-to-dsl.pl
	./obo-to-dsl.pl <$<
	touch $@

drum-dsl/UO/COMPLETE: downloads/unit.obo obo-to-dsl.pl
	./obo-to-dsl.pl <$<
	touch $@

drum-dsl/COMPLETE: drum-dsl/BTO/COMPLETE drum-dsl/CHEBI/COMPLETE drum-dsl/CO/COMPLETE drum-dsl/EFO/COMPLETE drum-dsl/GO/COMPLETE drum-dsl/PR/COMPLETE drum-dsl/UO/COMPLETE ncit-terms.tsv
	touch $@

# drum-dsl/MI/COMPLETE
# drum-dsl/SO/COMPLETE

clean::
	rm -rf specialist.tsv drum-terms.tsv drum-dbxrefs.tsv obo-terms.tsv hgnc-terms.tsv uniprot-terms.tsv uniprot-subcell-terms.tsv nextprot-family-terms.tsv ncit-terms.tsv pfam-terms.tsv mesh-scr-terms.tsv drum-dsl
endif

distclean:: clean
	rm -fr downloads

#
# Rules for Asma
#
ifneq (,$(wildcard asma.tsv))
install:: asma.tsv
	$(INSTALL_DATA) $< $(etcdir)/$(MODULE)
endif

#
# Rules for those using Stanford CoreNLP
#
ifneq (,$(wildcard Perl/TextTagger/StanfordCoreNLP.pm))
install::
	$(INSTALL_DATA) CoreNLP-to-TextTagger.xsl $(etcdir)/$(MODULE)
endif

#
# Rules for those using MetaMap
#
ifneq (,$(wildcard Perl/TextTagger/MetaMap.pm))
install::
	$(INSTALL_DATA) \
	  MetaMap-to-TextTagger.xsl \
	  SemGroups-to-LF-types.tsv \
	  SemTypes-to-LF-types.tsv \
	  NCI-codes-to-LF-types.tsv \
	  NCI-blacklist.txt \
	  NCI-whitelist.txt \
	  SNOMEDCT-codes-to-LF-types.tsv \
	  SNOMEDCT-blacklist.txt \
	  SNOMEDCT-whitelist.txt \
	  pseudo-meta-map.tsv \
	  $(etcdir)/$(MODULE)
endif

#
# Rules for MetaMapServer.pl
#
install-mms: MetaMapServer.pl install-umls-mysql-db install

start-mms: MetaMapServer.pl
	@case `uname` in \
	  Linux) screen -d -m script mms.log -a -f -c ./MetaMapServer.pl;;\
	  Darwin) screen -d -m script -a -t 0 mms.log ./MetaMapServer.pl;;\
	  *) echo "Unknown OS, please figure out the right arguments to pass to the script program and add it to the makefile"; exit 1;;\
	esac

stop-mms:
	@if test -r MetaMapServer.pid ; then \
	  kill -s INT `cat MetaMapServer.pid` ; \
	  sleep 5s ; \
	else \
	  echo "Can't find server PID" ; exit 1 ; \
	fi

restart-mms: stop-mms start-mms

mms-status:
	@if test 0 -eq `ps aux |fgrep MetaMapServer |fgrep -c -v fgrep` ; then \
	  echo "MetaMapServer.pl is NOT running" ; \
	else \
	  echo "MetaMapServer.pl is running (with the following number of processes)" ; \
	  ps aux |fgrep MetaMapServer |fgrep perl |fgrep -c -v fgrep ; \
	fi
	@if test 0 -eq `ps aux |fgrep wsd |fgrep -c -v fgrep` ; then \
	  echo "The WSD server is NOT running" ; \
	else \
	  echo "The WSD server is running" ; \
	fi
	@if test 0 -eq `ps aux |fgrep tagger |fgrep -c -v fgrep` ; then \
	  echo "The SKR MedPost server is NOT running" ; \
	else \
	  echo "The SKR MedPost server is running" ; \
	fi
	@tail -5000 mms.log |perl -n -e 's/\s+$$//; if (/^Script started on /) { $$c=0; } elsif (/\] Got connection from /) { $$c++; } elsif (/ exited with status /) { $$c--; } END { print "There are $$c active connections\n"; }'
	@echo "Last activity was" `tail -5000 mms.log |grep -o -P '^\[.*?\]' |tail -1`
	@if test 0 -eq `screen -ls |fgrep -c Attached` ; then \
	  echo "No screen is attached" ; \
	else \
	  echo "A screen is attached" ; \
	fi

install-umls-mysql-db: umls-tables.mysql $(TEXTTAGGER_umls)/SemGroups.txt
	(cd $(TEXTTAGGER_umls) && mysql -u root -p) <$<

install-umls-sqlite-db: umls-tables.sqlite $(TEXTTAGGER_umls)/SemGroups.txt
	(cd $(TEXTTAGGER_umls) && sqlite3 $(etcdir)/$(MODULE)/umls.db) <$<

# umls-tables.* assumes SemGroups is here (FIXME)
$(TEXTTAGGER_umls)/SemGroups.txt: $(TEXTTAGGER_semgroups)
	ln -s $< $@

#
# Rules for get-ssh-tunnel, needed for TextTagger server
#
get-ssh-tunnel: get-ssh-tunnel.c
	gcc get-ssh-tunnel.c -o get-ssh-tunnel

install-get-ssh-tunnel: get-ssh-tunnel
	# world readable and executable, setuid root
	$(INSTALL_PROGRAM) --group=root --owner=root --mode=4755 get-ssh-tunnel $(bindir)

