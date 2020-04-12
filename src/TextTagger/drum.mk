#
# Rules for Drum
#

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

drum-terms.tsv: merge-terms-files.pl obo-terms.tsv hgnc-terms.tsv uniprot-terms.tsv uniprot-subcell-terms.tsv nextprot-family-terms.tsv pfam-terms.tsv mesh-scr-terms.tsv ncit-terms.tsv hlsm-terms.tsv db-terms.tsv
	./$^ >$@

drum-dbxrefs.tsv: downloads/uniprot_sprot.dat.gz get-uniprot-dbxrefs.pl uniprot-species.pl
	gunzip -c $< \
	| ./get-uniprot-dbxrefs.pl >$@

OBO_FILES:=$(addprefix downloads/,famplex.obo BrendaTissueOBO cellosaurus.obo chebi.obo cl-basic.obo efo.obo go.obo pro_reasoned.obo unit.obo)
# psi-mi25.obo so-xp.obo

obo-terms.tsv: $(OBO_FILES) get-obo-terms.pl Perl/TextTagger/Normalize.pm Makefile
	TRIPS_BASE=$(prefix) ./get-obo-terms.pl $(OBO_FILES) >$@

go_protmods.obo: downloads/go.obo go_protmods.pl
	./go_protmods.pl -o $@ $<

go_protmods.tsv: go_protmods.obo get-obo-terms.pl Perl/TextTagger/Normalize.pm
	TRIPS_BASE=$(prefix) ./get-obo-terms.pl $< >$@

hgnc-terms.tsv: downloads/protein-coding_gene.txt.gz downloads/non-coding_RNA.txt.gz get-hgnc-terms.pl Perl/TextTagger/Normalize.pm
	( gunzip -c <downloads/protein-coding_gene.txt.gz && \
	  gunzip -c <downloads/non-coding_RNA.txt.gz \
	) | ./get-hgnc-terms.pl \
	>$@

uniprot-terms.tsv: downloads/uniprot_sprot.dat.gz get-uniprot-terms.pl Perl/TextTagger/Normalize.pm uniprot-species.pl
	gunzip -c <$< \
	| ./get-uniprot-terms.pl \
	>$@

uniprot-subcell-terms.tsv: downloads/subcell.txt get-uniprot-subcell-terms.pl Perl/TextTagger/Normalize.pm
	./get-uniprot-subcell-terms.pl <$< >$@

nextprot-family-terms.tsv: downloads/cv_family.txt get-nextprot-family-terms.pl Perl/TextTagger/Normalize.pm
	./get-nextprot-family-terms.pl <$< >$@

pfam-terms.tsv: downloads/Pfam-A.seed.gz get-xfam-terms.pl Perl/TextTagger/Normalize.pm
	gunzip -c <$< \
	| ./get-xfam-terms.pl \
	>$@

# get the current version of MeSH SCR terms, or use an old one if we have it
# already
MESH_FILE=$(shell cd downloads ; ls c20??.bin 2>/dev/null || date +c%Y.bin)
$(info MESH_FILE=$(MESH_FILE))

downloads/$(MESH_FILE):
	$(call download_compressed,ftp://nlmpubs.nlm.nih.gov/online/mesh/MESH_FILES/asciimesh/$(MESH_FILE))

mesh-scr-terms.tsv: downloads/$(MESH_FILE) get-mesh-scr-terms.pl Perl/TextTagger/Normalize.pm
	./get-mesh-scr-terms.pl <$< >$@

# get the current version of NCIt, since it changes so often and they don't
# keep the old ones available (or just use the one we have if we have it
# already)
NCIT_WEBSITE=http://evs.nci.nih.gov/ftp1/NCI_Thesaurus/
NCIT_ZIPFILE=$(shell cd downloads ; ls Thesaurus_??.???.FLAT.zip 2>/dev/null || (curl -s -L $(NCIT_WEBSITE) |grep -o -e 'Thesaurus_..\....\.FLAT.zip' |head -1))
$(info NCIT_ZIPFILE=$(NCIT_ZIPFILE))

# also writes drum-dsl/NCIT/*
ncit-terms.tsv: downloads/$(NCIT_ZIPFILE) get-ncit-terms.pl Perl/TextTagger/Normalize.pm
	./get-ncit-terms.pl $< >$@

hlsm-terms.tsv: downloads/small_molecule.csv get-hlsm-terms.pl Perl/TextTagger/Normalize.pm
	./get-hlsm-terms.pl $< >$@

specialist.tsv: downloads/LEXICON downloads/inflection.table $(prefix)/src/LexiconManager/Data/new get-specialist-tsv.pl Perl/TextTagger/Normalize.pm
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
	cut -d'|' -f1 <$< |perl -CSD -p -e "s/[^\\w']+/\\n/g;" |sort |uniq \
	|$(TEXTTAGGER_aspell) \
	  --local-data-dir="`$(TEXTTAGGER_aspell) config local-data-dir`" \
	  --encoding=utf-8 --lang=en create master ./$@ \
	2>specialist.aspell.err

drum.aspell: downloads/inflection.table drum-terms.tsv
	( cut -d'|' -f1 <downloads/inflection.table && \
	  cut -f1 <drum-terms.tsv \
	) | perl -CSD -p -e "s/[^\\w']+/\\n/g;" |sort |uniq \
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

# this started redirecting to something that causes SSL errors and has a different name
#downloads/pr.obo:
#	$(call download_compressed,"http://purl.obolibrary.org/obo/pr.obo")

downloads/pro_reasoned.obo:
	$(call download_insecure,"https://research.bioinformatics.udel.edu/PRO/data/current/pro_reasoned.obo")

downloads/unit.obo:
	$(call download_compressed,"https://raw.githubusercontent.com/bio-ontology-research-group/unit-ontology/master/unit.obo")

downloads/so-xp.obo:
	$(call download_sourceforge,"https://sourceforge.net/p/song/svn/HEAD/tree/trunk/so-xp.obo")

downloads/psi-mi25.obo:
	$(call download,"http://psidev.cvs.sourceforge.net/viewvc/*checkout*/psidev/psi/mi/rel25/data/psi-mi25.obo")

# see http://www.genenames.org/

downloads/protein-coding_gene.txt.gz:
	$(call download,"ftp://ftp.ebi.ac.uk/pub/databases/genenames/locus_groups/protein-coding_gene.txt.gz")

downloads/non-coding_RNA.txt.gz:
	$(call download,"ftp://ftp.ebi.ac.uk/pub/databases/genenames/locus_groups/non-coding_RNA.txt.gz")

# see http://www.uniprot.org/

downloads/uniprot_sprot.dat.gz:
	$(call download,"ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/complete/uniprot_sprot.dat.gz")

downloads/subcell.txt:
	$(call download,"https://www.uniprot.org/docs/subcell.txt")

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

drum-dsl/PR/COMPLETE: downloads/pro_reasoned.obo obo-to-dsl.pl
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

