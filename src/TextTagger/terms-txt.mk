#
# Rules for those using the GNIS geographic names database
#
all:: terms.txt

install:: terms.txt
	$(INSTALL_DATA) terms.txt $(etcdir)/$(MODULE)

clean::
	rm -rf terms.txt terms.unsorted.txt

terms.txt: make-terms-dot-txt.sh
	./make-terms-dot-txt.sh $(TEXTTAGGER_geonames)

