#
# defs.mk for config/lisp/mcl
#
# George Ferguson, ferguson@cs.rochester.edu, 12 Jan 2004
# Time-stamp: <Mon Jan 19 11:18:00 EST 2004 ferguson>
#

IMAGE = $(NAME).image

#
# usage: $(call compile-file,<filename>)
#
compile-file = $(LISP) --batch -e '(compile-file "$(1)")' -e '(quit)'

#
# usage: $(call compile-system,<defsys-filename>,<system-name>)
#
compile-system = $(LISP) --batch -l $(1) -e '(mk:compile-system $(2))' -e '(quit)'

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
