#
# File: Systems/drum/Makefile
#

NAME = trips-drum

include ../Makefile-common

$(IMAGE):: ../../TextTagger/drum-dsl-resources.lisp ../../TextTagger/drum-mappings.lisp

run-pmcid:
	$(MAKE) -f Makefile-run-pmcid

install:: install-configs run-pmcid
	$(MAKE) -f Makefile-run-pmcid install
	$(MKINSTALLDIRS) $(prefix)/www/cgi/ $(prefix)/www/pmc-oa-output/
	$(INSTALL_PROGRAM) run-pmcid $(prefix)/www/cgi/

install-configs:: TextTagger-*.conf xml-input-rules.txt DrumGUI.conf
	$(INSTALL_DATA) $^ $(etcdir)

