#
# defs.mk for config/lisp/ecl
#
# George Ferguson, ferguson@cs.rochester.edu, 13 Jun 2006
# Time-stamp: <Tue Jun 13 16:56:54 EDT 2006 ferguson>
#

# Not verified yet...

IMAGE = $(NAME).image

#
# usage: $(call compile-file,<filename>)
#
compile-file = $(LISP) -compile $(1) -eval '(quit)'

#
# usage: $(call compile-system,<defsys-filename>,<system-name>)
#
compile-system = $(LISP) -load $(1) -eval '(mk:compile-system $(2))' -eval '(quit)'

#
# usage: $(call dump-image,<load-filename>,<image-filename>,<toplevel-func>)
#
dump-image = $(LISP) --batch \
	-l $(1) \
	-e "(ccl:save-application \"$(2)\" :toplevel-function '$(3))" \
	-e '(quit)'

#
# usage: $(call dump-system,<defsys-filename>,<system-name>,<image-filename>,<toplevel-func>)
#
dump-system = $(LISP) --batch \
	-l $(1) -e '(mk:load-system $(2))' \
	-e "(ccl:save-application \"$(3)\" :toplevel-function '$(4))" \
	-e '(quit)'
