#
# defs.mk for config/lisp/clisp
#
# Nate Blaylock, blaylock@ihmc.us, 11 Apr 2007
# Time-stamp: <Wed Jan 21 16:11:59 EST 2004 ferguson>
#


IMAGE = $(NAME).mem

LISPCMD = $(LISP) 

#
# usage: $(call compile-file,foo.lisp)
#
compile-file = $(LISPCMD) -c $(1)

#
# usage: $(call compile-system,defsys.lisp,:widgetsys)
#
compile-system = $(LISPCMD) -i $(1) -x '(mk:compile-system $(2))'

#
# usage: $(call dump-image,load-file,image-filename,toplevel-func)
#
dump-image = $(LISPCMD) \
	-i $(1) \
	-x "(setq excl:*restart-app-function* '$(3)) (setq excl:*print-startup-message* nil) (dumplisp :name \"$(2)\" :suppress-allegro-cl-banner t)" 


#
# usage: $(call dump-system,defsys.lisp,sysname,image-filename,toplevel-func)
#
dump-system = $(LISPCMD) \
	-i $(1) \
	-x "(mk:load-system $(2)) (setq excl:*restart-app-function* '$(4)) (setq excl:*print-startup-message* nil) (dumplisp :name \"$(3)\" :suppress-allegro-cl-banner t)" 

#
# usage: $(call cleanup)
#
cleanup = rm -f `find . -name '*.$(FASL)' -o -name '*.mem' -o -name '*.lib'`
