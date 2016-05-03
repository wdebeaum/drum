
;;;; $Id: defsys.lisp,v 1.30 2015/05/22 14:22:56 wdebeaum Exp $

;;;;
;;;; defsys.lisp : Defsystem for the TRAINS/TRIPS parser
;;;;
;;;;
;;;; This file defines the PARSER package and the functions
;;;; PARSER::LOAD-SYSTEM and PARSER::COMPILE-SYSTEM.
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :comm)
  (load #!TRIPS"src;Comm;defsys"))

(unless (find-package :logging)
  (load #!TRIPS"src;Logging;defsys"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (mk::get-system :om)
  (load #!TRIPS"src;OntologyManager;defsys"))

(unless (mk::get-system :lxm)
  (load #!TRIPS"src;LexiconManager;defsys"))

(unless (find-package :parser)
  (load #!TRIPS"src;Parser;parser-pkg"))

(in-package :parser)

(mk:defsystem :grammar
    :source-pathname #!TRIPS"src;Parser;Grammar;"
    :load-only t
    :components ("hierarchical-features"
		 "phrase-grammar"
		 "time-grammar"
		 "clause-grammar"
		 "adverbial-grammar"
		 "procedures"
		 ))

(mk:defsystem :speech-grammar
  :source-pathname #!TRIPS"src;Parser;Grammar;"
    :load-only t
    :components ("speech-grammar"))

(mk:defsystem :definition-grammar
  :source-pathname #!TRIPS"src;Parser;Grammar;"
    :load-only t
    :components ("definition-grammar"))

(mk:defsystem :core-parser
    :source-pathname #!TRIPS"src;Parser;Core-parser;"
    :components ("structures"
		 "char"
		 "sem-features"
		 "trace"
		 "unify"
		 "FeatureHandling"
		 "probability"
		 "GrammarandLexicon"
		 "Chart"
		 "onlineParser"
		 "attachment"
		 "ontology-interface"
		 "printing"
		 "warning"
		 ))

(mk:defsystem :trips-parser
    :source-pathname #!TRIPS"src;Parser;Trips-parser;"
    :components ("streaminterface"
		 "tripsParser"
		 "attachments"
		 "messages"
		 ;;"speeling"
		 "support_funs"
		 "settings"
		;; "parserStats"
		 ))

(dfc:defcomponent :parser
    ;; From parser-pkg.lisp
    :use (:common-lisp :util :ontologymanager :lexiconmanager :w)
    :system (:depends-on (:comm :logging :util
				:om :lxm
				:core-parser :trips-parser)))

(dfc:defcomponent-method dfc:load-component :after ()
  (format *trace-output* "~&~%;;; load-component for parser...~%~%")
  (initialize))

(defvar *parser-package* (find-package "PARSER"))
(defvar *ont-package* (find-package "ONT"))
(defvar *w-package* (find-package "W"))

(defun initialize ()
  (init-parser)
  (load-grammar)
  (initialize-settings)
  )

(defun init-parser ()
  (format *trace-output* "~&~%;;; initializing parser...~%~%")
  ;; Set various initializations
  (disablegaps)
  (enablesem)
  (traceoff))

;; generic function for setting genre-specific settings, so new genres may be
;; defined later
(defgeneric do-genre-settings (genre))

(defmethod do-genre-settings ((genre null))
  ; do nothing
  )

(defmethod do-genre-settings ((genre (eql :speech)))
  (mk::load-system :speech-grammar)
  (setq *cost-table* *default-speech-cost-table*)
  (setq parser::*word-length* 6)
  (parser::setmaxnumberentries 1000)
  (parser::setmaxchartsize 1500)
  (setq lxm::*use-trips-and-wf-senses* nil)
  (setq lxm::*use-tagged-senses-only* nil)
  (setq lxm::*unknown-words-only* t)
  ;; WebParser needs this to always include W::lex, so it's set
  ;; in src/Systems/web-tools/system.lisp now
  ;(setq parser::*include-parse-tree-in-messages* nil)
  ;; setting *use-texttagger* here is a bad idea because it
  ;; affects which message sender the Parser pays attention to,
  ;; and this function is called as a result of a :genre setting
  ;; in such a message -- wdebeaum
  ;(setq user::*use-texttagger* t)
  (setq parser::*skeleton-boost-factor* 1.05)
  (setf (barrier-penalty *chart*) .9)
  )

(defmethod do-genre-settings ((genre (eql :text)))
  (setq *cost-table* *default-text-cost-table*)
  (setq parser::*word-length* 6)
  (parser::setmaxnumberentries 3000)
  (parser::setmaxchartsize 3000)
  (setq lxm::*use-trips-and-wf-senses* nil)
  (setq lxm::*use-tagged-senses-only* t)
  (setq lxm::*unknown-words-only* nil)
  ;(setq parser::*include-parse-tree-in-messages* nil)
  ;(setq user::*use-texttagger* t)
  (setq parser::*skeleton-boost-factor* 1.05)
  (setf (barrier-penalty *chart*) .95)
  )

(defmethod do-genre-settings ((genre (eql :definition)))
  (mk::load-system :definition-grammar)
  (setq *cost-table* *default-definition-cost-table*)
  (setq parser::*word-length* 5)
  (parser::setmaxnumberentries 1000)
  (parser::setmaxchartsize 1500)
  (setq lxm::*use-trips-and-wf-senses* nil)
  (setq lxm::*use-tagged-senses-only* t)
  ;(setq parser::*include-parse-tree-in-messages* '(W::lex W::passive))
  ;(setq user::*use-texttagger* nil)
  (setq parser::*skeleton-boost-factor* 1)  ;; ignoring stat parser for now
  (setf (barrier-penalty *chart*) 1)
  )

(defun load-grammar (&key (kr-in-lexicon lxm::*kr-in-lexicon*) 
		     (kr-boost lxm::*kr-sense-boost*) 
		     (no-kr-probability lxm::*no-kr-probability*)
		     (keep-nonspecialized lxm::*keep-nonspecialized-entries*)
		     (reinit-lexicon nil) 
		     (use-domain-senses lxm::*use-domain-senses*)
		     (genre nil)
		     )
  (format *trace-output* "~&~% ;;; loading grammar...~%~%")
  (when reinit-lexicon
    (om::initialize-ontology)
    (lxm::initialize-lexicon :kr-in-lexicon kr-in-lexicon :kr-boost kr-boost
			     :no-kr-probability no-kr-probability :keep-nonspecialized keep-nonspecialized
			     :use-domain-senses use-domain-senses)
    )
  (let ((*package* (find-package :parser)))
    (make-grammar nil)
    (init-parser-hierarchy)
    (init-foot-features)
    (mk:load-system :grammar)
    (do-genre-settings genre)
    (build-grammar-indices *skeleton-constit-cats*)
    nil
    ))

(defun init-parser-hierarchy ()
  (init-type-hierarchy)

  ;; Set up the SEM structures in the parser
  (apply #'compile-sem-features
	 (ontologymanager::get-defined-sem-features))
  
  (add-ontology-hierarchical-features (get-sem-features))
  
  )

;; legacy function...
(defun run ()
  (dfc:run-component :parser))
