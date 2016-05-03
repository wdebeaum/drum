# Makefile
#
# Time-stamp: <Wed Dec 17 09:38:52 CST 2014 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  7 Dec 2014
#

default all: symlinks

clean: clean-symlinks

symlinks: .symlinks
	-cat $< \
	| sed 's/->/ /' \
	| perl -ne '($$link, $$file) = split(/\s+/); `ln -fs $$file $$link`;'

clean-symlinks: .symlinks
	-cat $< \
	| awk '{print $$1}' \
	| xargs rm 
