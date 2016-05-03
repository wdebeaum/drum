#
# defs.mk for config/lisp/sbcl
#
# George Ferguson, ferguson@cs.rochester.edu, 30 Jan 2007
# Time-stamp: <Tue Mar 15 00:09:41 CDT 2016 lgalescu>
#

IMAGE = $(NAME).core

LISPCMD = $(LISP) --no-userinit --disable-debugger

#
# usage: $(call compile-file,foo.lisp)
#
compile-file = $(LISPCMD) --eval '(compile-file "$(1)" :print nil :verbose nil)' --eval '(quit)'

#
# usage: $(call compile-system,defsys.lisp,:widgetsys)
#
compile-system = $(LISPCMD) --load $(1) --eval '(mk:compile-system $(2))' --eval '(quit)'

#
# usage: $(call dump-image,load-file,image-filename,toplevel-func)
#
dump-image = $(LISPCMD) \
	--load $(1) \
	--eval "(sb-ext:save-lisp-and-die \"$(2)\" :toplevel '$(3))"

#
# usage: $(call dump-system,defsys.lisp,sysname,image-filename,toplevel-func)
#
dump-system = $(LISPCMD) \
	--load $(1) --eval '(mk:load-system $(2))' \
	--eval "(sb-ext:save-lisp-and-die \"$(3)\" :toplevel '$(4))"

#
# usage: $(call cleanup)
#
cleanup = rm -f `find . -name '*.$(FASL)' -o -name '*.core' -o -name '*.err'`
