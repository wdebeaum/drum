#
# defs.mk for config/lisp/cmucl
#
# George Ferguson, ferguson@cs.rochester.edu, 12 Jan 2004
# Time-stamp: <Tue Jan 20 13:50:05 EST 2004 ferguson>
#

IMAGE = $(NAME).core

LISPCMD = $(LISP) -batch -noinit -eval '(setq ext:*gc-verbose* nil)' -eval '(setq ext:*undefined-warning-limit* 0)'

#
# usage: $(call compile-file,foo.lisp)
#
compile-file = $(LISPCMD) -eval '(compile-file "$(1)" :print nil :verbose nil)' -eval '(quit)'

#
# usage: $(call compile-system,defsys.lisp,:widgetsys)
#
compile-system = $(LISPCMD) -load $(1) -eval '(mk:compile-system $(2))' -eval '(quit)'

#
# usage: $(call dump-image,load-file,image-filename,toplevel-func)
#
dump-image = $(LISPCMD) \
	-load $(1) \
	-eval "(extensions:save-lisp \"$(2)\" :init-function '$(3))" \
	-eval '(quit)'

#
# usage: $(call dump-system,defsys.lisp,sysname,image-filename,toplevel-func)
#
dump-system = $(LISPCMD) \
	-load $(1) -eval '(mk:load-system $(2))' \
	-eval "(extensions:save-lisp \"$(3)\" :init-function '$(4))" \
	-eval '(quit)'

#
# usage: $(call cleanup)
#
cleanup = rm -f `find . -name '*.$(FASL)' -o -name '*.core' -o -name '*.err'`
