CONFIGDIR=../../config
include $(CONFIGDIR)/perl/defs.mk
include $(CONFIGDIR)/Graphviz/defs.mk

ifneq ($(DOT_BIN_DIR),)
  PATH ::= $(DOT_BIN_DIR):$(PATH)
endif
ifneq ($(DOT_LIB_DIR),)
  ifeq ($(LD_LIBRARY_PATH),)
    LD_LIBRARY_PATH ::= $(DOT_LIB_DIR)
  else
    LD_LIBRARY_PATH ::= $(DOT_LIB_DIR):$(LD_LIBRARY_PATH)
  endif
endif

all: tagger-deps.svg

tagger-deps.svg: get-tagger-deps.pl
	$(PERL) $< |dot -Tsvg >$@

clean:
	rm -f tagger-deps.svg
