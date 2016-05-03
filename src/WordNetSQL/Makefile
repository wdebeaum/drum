# WordNetSQL Makefile
# William de Beaumont
# $Date: 2015/10/28 20:41:08 $

CONFIGDIR=../config
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/perl/defs.mk
-include $(CONFIGDIR)/ruby/defs.mk
WORDNET=$(shell grep wordnet-basepath $(CONFIGDIR)/WordFinder/defs.lisp | $(PERL) -p -e 's/^.*?"//; s/".*//; s/\/dict\/?$$//;')
GLOSSTAG=$(WORDNET)/glosstag/standoff/
SENSEMAP=$(WORDNET)/sensemap/

ESWN=$(WORDNET)/../esWN-200611

all: wn.db

install: install-wn install-lib

docs: rdoc pod

install-lib: word_net_sql.polyglot $(etcdir)/WordNetSQL
	$(INSTALL_DATA) $< $(etcdir)/WordNetSQL/
	# placate naming conventions for Perl and Ruby libraries
	ln -sf $< $(etcdir)/WordNetSQL/WordNetSQL.pm
	ln -sf $< $(etcdir)/WordNetSQL/word_net_sql.rb

install-wn: wn.db $(etcdir)/WordNetSQL 
	# this file is ~300MB, so try to only keep one copy of it
	if [ ! -L wn.db ] ; then \
	  mv wn.db $(etcdir)/WordNetSQL/ ; \
	  ln -s $(etcdir)/WordNetSQL/wn.db ./ ; \
	fi

install-eswn: eswn.db $(etcdir)/WordNetSQL
	$(INSTALL_DATA) $< $(etcdir)/WordNetSQL/

install-sense-map: sense-map.db $(etcdir)/WordNetSQL
	$(INSTALL_DATA) $< $(etcdir)/WordNetSQL/

$(etcdir)/WordNetSQL:
	$(MKINSTALLDIRS) $(etcdir)/WordNetSQL/

ifeq (,$(wildcard download-wn-db.sh))

# can't download wn.db, so actually make it locally
wn.db: wn.db-no-really

else

# by default, just download wn.db, because making it takes a long time
wn.db: download-wn-db.sh
	./download-wn-db.sh

endif

# use this target if you actually want to make wn.db for yourself
wn.db-no-really: make-wordnet-sql-db.pl glosstags.sqlite glosstag-standoff-to-sql.pl glosstag-standoff-to-sql.xsl get-del-ins.ph fix-glosstag-offsets-manually.pl manual-glosstag-fixes.txt
	rm -f wn.db
	$(PERL) make-wordnet-sql-db.pl $(WORDNET) wn.db
	sqlite3 wn.db <glosstags.sqlite
	$(PERL) glosstag-standoff-to-sql.pl $(GLOSSTAG) wn.db >gsts-warnings.txt
	$(PERL) fix-glosstag-offsets-manually.pl wn.db <manual-glosstag-fixes.txt

eswn.db: eswn.sqlite eswn-pointer-symbols.psv eswn-pointer-inverses.psv eswn-variant.psv eswn-synset.psv eswn-relation.psv
	sqlite3 $@ <$<

eswn-synset.psv: $(ESWN)/esWN-200611-synset standardize-eswn-glosses.pl
	iconv -f LATIN1 -t UTF-8 <$< \
	| $(PERL) standardize-eswn-glosses.pl \
	>$@

# downcase lemmas
eswn-variant.psv: $(ESWN)/esWN-200611-variant
	iconv -f LATIN1 -t UTF-8 <$< \
	| $(PERL) -CS -p -e '$$_=lc;' \
	>$@

eswn-%.psv: $(ESWN)/esWN-200611-%
	iconv -f LATIN1 -t UTF-8 <$< >$@

sense-map.db: sense-map.sqlite sense-map.psv
	sqlite3 $@ <$<

sense-map.psv: sense-map-to-psv.pl $(SENSEMAP)
	$(PERL) sense-map-to-psv.pl $(SENSEMAP)/*to*.{noun,verb}.{mono,poly} >$@

rdoc: word_net_sql.polyglot
	rm -rf rdoc
	if test -n "$(RDOC)" ; then $(RDOC) -E polyglot=rb -o $@ $< ; fi

pod: word_net_sql.polyglot
	mkdir -p pod
	fgrep -v REMOVE_THIS_LINE_FOR_POD <$< >pod/WordNetSQL.pm
	cd pod ; \
	pod2man WordNetSQL.pm >WordNetSQL.man ; \
	pod2html --infile=WordNetSQL.pm --outfile=WordNetSQL.html \
		--title=WordNetSQL

clean:
	rm -f gsts-warnings.txt \
	      eswn.db eswn-{variant,synset,relation}.psv \
	      sense-map.{db,psv} \
	      rdoc pod

# wn.db is large and unlikely to need remaking, so we don't clean it unless we want distclean!
distclean:
	rm -f wn.db
