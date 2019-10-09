#
# Rules for those using the WordNet multiwords tagger
#

all:: wn-multiwords.tsv

install:: wn-multiwords.tsv
	$(INSTALL_DATA) wn-multiwords.tsv $(etcdir)/$(MODULE)

clean::
	rm -rf wn-multiwords.tsv

wn-multiwords.tsv: get-wn-multiwords.pl $(prefix)/etc/WordNetSQL/wn.db $(prefix)/etc/WordNetSQL/WordNetSQL.pm $(prefix)/etc/util/add_suffix.ph ../WordFinder/wordnet/stoplist.txt
	TRIPS_BASE=$(prefix) $(PERL) ./$< >$@

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
