#
# configuration
#
CONFIGDIR=../config
include $(CONFIGDIR)/perl/defs.mk

# directory where CGI scripts will live
CGI_DIR=/Library/WebServer/Documents/WordNetWeb/

# relative path from $(CGI_DIR) to where the style files will live
# this must work from the perspectives of both the browser and the server
STYLE_RELATIVE_PATH=

#
# classes of file to install
#

# these get put in $(CGI_DIR)$(STYLE_RELATIVE_PATH)
STYLE_FILES=wn-xml-to-xhtml.xsl wn.css wn.js
# these get put in $(CGI_DIR) and made executable
CGI_SCRIPTS=get-synset-xml.pl get-word-xml.pl
# these get put in $(CGI_DIR) but not made executable
OTHER_FILES=../WordNetSQL/wn.db ../WordNetSQL/eswn.db ../WordNetSQL/sense-map.db get-xml.ph add_suffix.ph
# an .htaccess file also gets put in $(CGI_DIR) if $(STYLE_RELATIVE_PATH) is empty

#
# rules
#

all: add_suffix.ph

add_suffix.ph:
	ln -sf ../util/add_suffix.polyglot $@

install: all
	mkdir -p $(CGI_DIR)
	mkdir -p $(CGI_DIR)$(STYLE_RELATIVE_PATH)
	for f in $(OTHER_FILES) ;\
	do \
	  if [ -e "$$f" ] ;\
	  then \
	    cp $$f $(CGI_DIR) ;\
	  fi ;\
	done
	if [ -z "$(STYLE_RELATIVE_PATH)" ] ;\
	then \
	  cp dot-htaccess $(CGI_DIR)/.htaccess ;\
	  echo "AuthUserFile `pwd`/htdigest" >>$(CGI_DIR)/.htaccess ;\
	fi
	for f in $(CGI_SCRIPTS) ;\
	do \
	  $(PERL) -p -e 'BEGIN { @style_files = (qw($(STYLE_FILES))); $$style_files = join("|", @style_files); } s/"($$style_files)"/"$(STYLE_RELATIVE_PATH)$$1"/g; s@#!/usr/bin/perl@#!$(PERL)@;' <$$f >$(CGI_DIR)$$f ;\
	  chmod a+x $(CGI_DIR)$$f ;\
	done
	for f in $(STYLE_FILES) ;\
	do \
	  $(PERL) -p -e 'BEGIN { @style_files = (qw($(STYLE_FILES))); $$style_files = join("|", @style_files); } s/"($$style_files)"/"$(STYLE_RELATIVE_PATH)$$1"/g;' <$$f >$(CGI_DIR)$(STYLE_RELATIVE_PATH)$$f ;\
	done

clean:
	# nothing to clean

