#
# Rules for those using terms.c (see also several sections using this below)
#
all:: terms

install:: terms
	$(INSTALL_PROGRAM) terms $(bindir)

clean::
	rm -rf terms

terms: terms.c
	gcc terms.c -o terms

