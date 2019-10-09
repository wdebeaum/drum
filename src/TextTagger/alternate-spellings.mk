#
# Rules for those using the American/British alternate spellings list
#
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

