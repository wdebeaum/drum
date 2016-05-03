NAME = DeepSemLex
DEFSYS = defsys.lisp
SYSTEM = :deepsemlex

CONFIGDIR=../../../config
include $(CONFIGDIR)/lisp/defs.mk

# Hack to ensure things get rebuilt when any .lisp file is newer than any .fasl
# file (which is overkill, but not by much). If a .lisp file that defines types
# or macros changes, any other .lisp file that uses them needs recompiling,
# even if that file didn't change. Lisp's build system doesn't take this into
# account, so we need this hack.
default all compile install::
	if [ "`ls -t *.lisp |head -1`" -nt "`ls -tr *.$(FASL) |head -1`" ] ; then \
	  rm -f *.$(FASL) ; \
	fi

include $(CONFIGDIR)/lisp/prog.mk

test:
	echo '(load "tests") (quit)' | $(LISPCMD)

