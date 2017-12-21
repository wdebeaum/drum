
(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up :up "config" "lisp")
		       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (fboundp 'util::add-suffix)
  (load #!TRIPS"src;util;add_suffix.polyglot"))

#| someday we'll replace the functions LXM exports like this
(defpackage :deepsemlex
  (:use :common-lisp :util)
  (:nicknames :dsl)
  (:export
    initialize-lexicon
    get-word-def
    retrieve-from-lex
    initialize-ontology
    subtype
    subtype-in
    )
  )
|#

(dfc:defcomponent :deepsemlex
                  :nicknames (:dsl)
		  :use (:util :common-lisp)
		  :system (
		    :depends-on (:util :comm)
				:components (
					     "mop"
					     "lisp-types"
					     "unify"
					     "resources"
					     "symbol-types"
					     "generics"
					     "classes"
					     "load"
					     "load-old"
					     "print"
					     "make-db"
					     "query"
					     "messages"
					     )))

;;; Define packages used in data files (including those normally defined
;;; elsewhere, because DSL might be running standalone).

;; The Lisp spec says the consequences of calling defpackage on a preexisting
;; package with a different definition are undefined. This is a safer version
;; of defpackage that just adds to the :use list in that case.
(defmacro def-or-update-package (defined-package-name &body options)
  `(let ((old-package (find-package ',defined-package-name)))
     (cond
       (old-package
	 (use-package ',(cdr (assoc :use options)) old-package)
	 old-package)
       (t ; new package
	 (defpackage ,defined-package-name ,@options))
       )))

(def-or-update-package :lxm
  (:use :dsl)
  (:nicknames :lexiconmanager)
  )

(def-or-update-package :ontologymanager
  (:use :dsl)
  (:nicknames :om)
  )

(def-or-update-package :w (:use))
(def-or-update-package :f (:use)) ; should go away at some point...
;(def-or-update-package :ont) ; see resources.lisp
(defpackage :lexicon-data
  (:use)
  (:nicknames :ld)
  )

(defpackage :type-predicates)

(defvar *db* nil) ; set in make-db.lisp
