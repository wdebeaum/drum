MODULE=TextTagger/terms2
PROG=terms2
SRCS=terms2.cpp bloom_filter.cpp MurmurHash3.cpp
HDRS=bloom_filter.h MurmurHash3.h

CONFIGDIR=../../config
include $(CONFIGDIR)/c++/prog.mk
include $(CONFIGDIR)/TextTagger/defs.mk

EXTRA_CXXFLAGS=$(TEXTTAGGER_CXXFLAGS)
EXTRA_LDFLAGS=$(TEXTTAGGER_LDFLAGS)

