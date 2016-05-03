#
# defs.mk for config/lisp/allegro
#
# George Ferguson, ferguson@cs.rochester.edu, 21 Jan 2004
# Time-stamp: <Wed Jan 21 16:11:59 EST 2004 ferguson>
#

IMAGE = $(NAME).dxl

LISPCMD = $(LISP) -batch -q 

#
# usage: $(call compile-file,foo.lisp)
#
compile-file = $(LISPCMD) -e '(compile-file "$(1)")' -e '(exit)'

#
# usage: $(call compile-system,defsys.lisp,:widgetsys)
#
compile-system = $(LISPCMD) -L $(1) -e '(mk:compile-system $(2))' -e '(exit)'

#
# usage: $(call dump-image,load-file,image-filename,toplevel-func)
#
dump-image = $(LISPCMD) \
	-L $(1) \
	-e "(setq excl:*restart-app-function* '$(3))" \
	-e "(setq excl:*print-startup-message* nil)" \
	-e "(dumplisp :name \"$(2)\" :suppress-allegro-cl-banner t)" \
	-e '(exit)'

#
# usage: $(call dump-system,defsys.lisp,sysname,image-filename,toplevel-func)
#
dump-system = $(LISPCMD) \
	-L $(1) -e '(mk:load-system $(2))' \
	-e "(setq excl:*restart-app-function* '$(4))" \
	-e "(setq excl:*print-startup-message* nil)" \
	-e "(dumplisp :name \"$(3)\" :suppress-allegro-cl-banner t)" \
	-e '(exit)'

#
# usage: $(call cleanup)
#
cleanup = rm -f `find . -name '*.$(FASL)' -o -name '*.dxl'`
