CONFIGDIR=../../config
include $(CONFIGDIR)/defs.mk

LANGUAGES = java javascript

test_for_language = test -f Makefile-$(1) -a -d $(CONFIGDIR)/$(1:objc=c)

all: $(LANGUAGES)

$(LANGUAGES):
	@if $(call test_for_language,$@); then $(MAKE) -f Makefile-$@; fi

clean install install.man:
	@for l in $(LANGUAGES); do \
	  if $(call test_for_language,$$l); then $(MAKE) -f Makefile-$$l $@; fi; \
	done

