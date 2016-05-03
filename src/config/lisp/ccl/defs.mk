#
# File: config/lisp/ccl/defs.mk
# Creator: George Ferguson
# Created: Tue Jan 19 15:48:53 2010
# Time-stamp: <Tue Jan 19 15:49:30 EST 2010 ferguson>
#
# Based on openmcl/defs.mk, which in turn derived from mcl/defs.mk
#

IMAGE = $(NAME).image

LISPCMD = $(LISP) --batch --no-init

#
# usage: $(call compile-file,foo.lisp)
#
compile-file = $(LISPCMD) -e '(compile-file "$(1)")' -e '(quit)'

#
# usage: $(call compile-system,defsys.lisp,:widgetsys)
#
compile-system = $(LISPCMD) -l $(1) -e '(mk:compile-system $(2))' -e '(quit)'

#
# usage: $(call dump-image,load-file,image-filename,toplevel-func)
#
dump-image = $(LISPCMD) \
	-l $(1) \
	-e "(ccl:save-application \"$(2)\" :toplevel-function '$(3))" \
	-e '(quit)'

#
# usage: $(call dump-system,defsys.lisp,sysname,image-filename,toplevel-func)
#
dump-system = $(LISPCMD) \
	-l $(1) -e '(mk:load-system $(2))' \
	-e "(ccl:save-application \"$(3)\" :toplevel-function '$(4))" \
	-e '(quit)'

#
# usage: $(call cleanup)
#
cleanup = rm -f `find . -name '*.$(FASL)' -o -name '*.image'`
