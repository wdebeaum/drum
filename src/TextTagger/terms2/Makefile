MODULE=TextTagger/terms2
PROG=terms2
SRCS=terms2.cpp bloom_filter.cpp MurmurHash3.cpp
HDRS=bloom_filter.h MurmurHash3.h

CONFIGDIR=../../config
include $(CONFIGDIR)/c++/prog.mk
include $(CONFIGDIR)/TextTagger/defs.mk

# FIXME? maybe should call pkg-config in configure instead
EXTRA_CXXFLAGS=$(TEXTTAGGER_CXXFLAGS) $(shell pkg-config --cflags icu-uc)
EXTRA_LDFLAGS=$(TEXTTAGGER_LDFLAGS)
EXTRA_LIBS=$(shell pkg-config --libs icu-uc)

