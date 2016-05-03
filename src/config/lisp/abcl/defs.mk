#
# defs.mk for config/lisp/abcl
#
# George Ferguson, ferguson@cs.rochester.edu, 3 Feb 2007
# Time-stamp: <Fri Feb  2 14:35:01 EST 2007 ferguson>
#
# Arguments for abcl seem to be modelled on sbcl.
# Haven't tested any kind of dump...
#

IMAGE = $(NAME).core

LISPCMD = $(LISP)

#
# usage: $(call compile-file,foo.lisp)
#
compile-file = $(LISPCMD) --eval '(compile-file "$(1)" :print nil :verbose nil)' --eval '(quit)'

#
# usage: $(call compile-system,defsys.lisp,:widgetsys)
#
compile-system = $(LISPCMD) --eval '(load "$(1)")' --eval '(mk:compile-system $(2))' --eval '(quit)'

#
# usage: $(call dump-image,load-file,image-filename,toplevel-func)
#
dump-image = $(LISPCMD) \
	--eval '(load "$(1)")' \
	--eval "(error \"no dump-image defined for abcl\" \"$(2)\" :toplevel '$(3))"

#
# usage: $(call dump-system,defsys.lisp,sysname,image-filename,toplevel-func)
#
dump-system = $(LISPCMD) \
	--eval '(load "$(1)")' --eval '(mk:load-system $(2))' \
	--eval "(error \"no dump-image defined for abcl\" \"$(3)\" :topllevel '$(4))"

#
# usage: $(call cleanup)
#
cleanup = rm -f `find . -name '*.$(FASL)'`
