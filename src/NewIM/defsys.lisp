;;;;
;;;; defsys.lisp : Defsystem for the TRAINS/TRIPS IM
;;;;
;;;; $Id: defsys.lisp,v 1.16 2016/01/01 16:00:39 james Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :comm)
  (load #!TRIPS"src;Comm;defsys"))

(unless (find-package :logging)
  (load #!TRIPS"src;Logging;defsys"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (find-package :ont)
  (load #!TRIPS"src;OntologyManager;ont-pkg"))

(unless (find-package  :parser)
  (load #!TRIPS"src;Parser;defsys"))

(defpackage :im
  (:use :common-lisp :util :dfc :parser))
  ;;(:export "get-KB-def"))

(in-package :im)

(mk:defsystem :im-code
    :source-pathname #!TRIPS"src;NewIM;"
    :components ("structures"
		 "messages"
		 "DialogStateManager"
		 "MatchEngine"
		 "LFstore"
		 "Manager"
		 "reference"
		 "visual-reference"
		 "KBreference"
		 "channel-manager"
		 "warn"
		 "IM-Managers" 
		 "SpeechActAnalysis"
		 "extract"
		 "temporal-relation"
		 
		 ))
 
(mk:defsystem :im-data
    :source-pathname #!TRIPS"src;NewIM;"
    :components ("IMrules"
		 ;;"protocols"
		 ))

(mk:defsystem :parser-fns
    :source-pathname #!TRIPS"src;NewIM;ParserCopy;"
    :components ("parserstructures"
		 "semfeatures"
		 "unify"
		 "printing"
		 "trace"
		 "GrammarandLexicon"
		 "Chart"
		 "ontology-interface"
		 "warning"
		 "Trips-parser.lisp"
		 "FeatureHandling"
		 ))


;;(mk:defsystem :im
(dfc:defcomponent :im
    :use (:util :common-lisp)
    :system (:depends-on (:comm :logging :util :parser-fns
		 :parser :om ;; but the parser will load that
		 :im-code :im-data)))

;; Default agents, set to right values in (initialize)
(defvar *me* 'ONT::SYS)

(defvar *user* 'ONT::USER)

(defvar *im-package* (find-package :im))

(defvar *BA-develop-mode* nil
  "When *BA-DEVELOP-MODE* is set, the IM calls the fake BA instead of
the real ones.")

(defvar *external-name-resolution* nil)  ;; set to T if we do an external message to resolve the name

(defvar *max-allowed-utts-in-turn* 2)

(defvar *allow-optional-lfs* nil)

(defvar *symbol-map* nil)

(when *BA-develop-mode*
  (format *error-output* "~%~%warning: IM in develop mode. To disable this set *develop-mode* to nil~%~%"))

(dfc:defcomponent-method dfc:init-component :after ()
  (initialize))

(defun run ()
  (dfc:run-component :im))

;; control variables

(defvar *show-lf-graphs* nil)

(defun initialize nil
  (defvar *current-dialogue-manager* #'textIM)  ;; the default dialogue manager. This should be set in the system.lisp file - its set here just in case its not!
 (om::initialize-ontology)
 (init-ont-hierarchy)
 
 )

(defvar *addressee* nil)

(defvar *kr-package* (find-package :ont))

(defun init-ont-hierarchy ()
  (init-type-hierarchy)

  ;; Set up the SEM structures in the parser
  (apply #'compile-sem-features
	 (ontologymanager::get-defined-sem-features))
  (declare-ontology-hierarchical-features '(w::sem w::role w::class))
  (add-ontology-hierarchical-features (get-sem-features))
  
  )

(defvar *ontology-package* (find-package :ont))
