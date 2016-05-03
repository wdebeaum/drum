;;;;
;;;; defsys.lisp : Defsystem for the TRIPS utility routines in Lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 13 Aug 1999
;;;; $Id: defsys.lisp,v 1.12 2010/06/25 19:05:51 hjung Exp $
;;;;
;;;; This file defines the UTIL package and the functions LOAD-UTIL
;;;; and COMPILE-UTIL
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :logging)
  (load #!TRIPS"src;Logging;defsys"))

(defpackage :util
  (:use :common-lisp)
  (:export ;;find-type-in-spec
          generate-akrl-relns-from-specs
	   keywordify
	   flatten
	   reuse-cons
	   append-if-necessary
	   split-list
	   ;; flatten-ands
	   sa-type
	   find-arg
	   find-all-args-for-slot
	   find-arg-in-act
           insert-arg-in-act
	   remove-arg
	   remove-args
	   remove-arg-in-act
	   replace-arg
	   replace-arg-in-act
	   merge-feature-lists
	   merge-values
	   equal-values
	   right-var
	   is-variable-name
	   is-positive-variable-name
	   is-variable-symbol
	   is-trips-variable
	   merge-features
	   merge-values
	   merge-in-defaults
	   merge-in-feature
	   sethash
	   print-hash-table
	   get-all-keys
	   get-all-values
	   mmapcan
	   make-system-condition
	   generate-subsets
	   memberp
	   assocval
	   make-pair-list
	   read-in-list-from-stream
	   tree-find-if
	   strip-variable-sign
	   empty-string
	   convert-to-package
	   reuse-cons
	   upcase-symbol-names
	   ;; AKRL functions
	   get-def-from-akrl
	   get-def-from-akrl-context
	   remove-def-from-akrl
	   remove-def-from-akrl-context
	   get-role-from-akrl
	   get-roles-from-def
	   get-role-from-akrl-context
	   add-role-to-akrl-context
	   replace-def-in-akrl-context
	   remove-unused-context-with-root
	   remove-unused-context
	   remove-unused-terms
	   replace-reln-role
	   get-reln-role
	   akrl-match
	   akrl-match-into-context
	   split-list
	   gen-symbol
           get-ids

	   ;; string.lisp
	   alphanumeric-stringp
	   numeric-stringp
	   replace-hyphens
	   normalize-string
	   remove-punctuation
	   replace-&
	   replace-spaces
	   replace-space-with-hyphen
	   crop-string
	   split-string
	   count-words
	   number-to-string
	   number-to-strlist
	   base-num-to-string
	   unit-position
		 ))

(in-package :util)

(mk:defsystem :util
    :source-pathname #!TRIPS"src;util;"
    :components ((:file "warning")
		 (:file "util")
		 (:file "argumentutil")
		 (:file "string")))

(defun load-system (&key (verbose t))
  (let ((*load-verbose* verbose))
    (mk:load-system :logging :minimal-load t)
    (mk:load-system :util)))

(defun compile-system (&key (verbose nil))
  (let ((*compile-verbose* verbose)
	(*compile-print* verbose)
	(*load-verbose* verbose))
    (mk:compile-system :util)))

