;; Nate Chambers
;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :comm)
  (load #!TRIPS"src;Comm;defsys"))

(unless (find-package :logging)
  (load #!TRIPS"src;Logging;defsys"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (find-package :simplekb)
  (load #!TRIPS"src;SimpleKB;defsys"))

(unless (find-package :ont)
  (load #!TRIPS"src;OntologyManager;ont-pkg"))

(unless (find-package :f)
  (load #!TRIPS"src;OntologyManager;f-pkg"))

(unless (find-package :w)
  (load #!TRIPS"src;LexiconManager;w-pkg"))

;; Some LFMapper functions call LxM functions directly...
(unless (find-package :lxm)
  (load #!TRIPS"src;LexiconManager;lxm-pkg"))

(unless (find-package :om)
  (load #!TRIPS"src;OntologyManager;om-pkg"))

(in-package :ontologymanager)

(defvar *ont-package* (find-package :ont))
(defvar *lf-ontology* nil)
(defvar *kr-ontology* nil)

(defvar *trap-bad-entries* nil
  "Set to t to enter a break when a bad call to type subsystem.")
(defvar *trace-transform-process* nil
  "Set to t to trace how the transforms are found.")
(defvar *more-specific-preference-adjustment* 0.999
  "This is a variable that controls the application of the transforms.
They get preferences based on inclusion, and the preferences of the
supertype transforms are adjusted by this value to ensure that they don't
apply first.")

;; This flag is for LFMapper and refers to an arbitrary KR, not necessarily the
;; TRIPS KR.
;; This flag indicates if we have access to the KR for type information.
;; If set to true, the function query-kr in messaging.lisp will be
;; called to determine type information.
(defvar *kr-available-for-mapping* nil)
;; Global assoc list that holds the transform rules in memory.
(defvar *lftransforms* '(nil nil))
;; Any debug statement of level below *debug-level* will be output
;; i.e. *debug-level*=0 stops all debugging output
(defvar *debug-level* 0)

; moved to the LexiconManager
;(defvar *domain-sense-preferences* nil) ; set with domain-specific word senses if available in domain directory (default is nil): Data/Domains/. This var is only used by LXM so ideally should be there. Currently here to use the old domain-specific loading mechanisms in OM until a new improved method is implemented.


;; LFMapper
(mk:defsystem :transform-overhead
	      :source-pathname #!TRIPS"src;OntologyManager;Code;LFMapper;"
	      :components ("messaging"
			   ))
;; LFMapper
(mk:defsystem :transform-code
	      :source-pathname #!TRIPS"src;OntologyManager;Code;LFMapper;"
	      :components ("structures"
			   "rule-reader"
			   "common"
			   "transform"
			   "syntax-transform"
			   "kr-transform"
			   "indexing"
			   "reverse"
			   )
	      :depends-on (:transform-overhead)
	      )

(mk:defsystem :lfontology-data
    :source-pathname #!TRIPS"src;OntologyManager;Data;LFdata;"
    :load-only t
    :components ("feature-declarations"
		 "feature-types"
		 "root-types"
		 "speech-acts"
		 "predicates"
		 "time-location-types"
		 "physobj"
		 "situation-types"
		 "abstract-types"
		 "property-val"
                 "domain-and-attribute-types"
		 "specific-situation-types"
		 "social-contract"
                 "music-terminology"
		 ))

(mk:defsystem :ontology-code-lf
    :source-pathname #!TRIPS"src;OntologyManager;Code;LFOntology;"
    :components ("structures"
		 "warning"
		 "common-functions"
		 "feature-handling"
		 "feature-type-handling"
		 "lf-handling"
		 "interface")
    ;;    :depends-on (:ontology-code-kr)
    )

(mk:defsystem :ontology-code-kr
    :source-pathname #!TRIPS"src;OntologyManager;Code;KROntology;"
    :components ("structures"
		 "warning"
		 "hierarchy-functions"
		 "class-handling"
		 "predicate-handling"
		 "operator-handling"
		 ;;"mapping-handling"
		 "transform-handling"
		 "transform-application"
		 "transforms"
		 "interface"
		 )
    :depends-on (:ontology-code-lf)
    )
    
(mk:defsystem :ontology-manager-interface
    :source-pathname #!TRIPS"src;OntologyManager;Code;"
    :components ("interface"
		 "interface-helper")    
    :depends-on (:ontology-code-lf :ontology-code-kr))

#|(mk:defsystem :ontology-manager-overhead
    :source-pathname #!TRIPS"src;OntologyManager;"
    :components ("macros"
		 "messages")
    :depends-on (:ontology-code-lf :ontology-code-kr))

(mk:defsystem :ontology-code
    :depends-on (:ontology-manager-interface :ontology-manager-overhead
		 :ontology-code-lf :ontology-code-kr))

(mk:defsystem :om
    :package :om
    :depends-on (:comm :logging :util :simplekb
		 :transform-overhead :transform-code
		 :ontology-manager-interface
		 :ontology-manager-overhead
		 :ontology-code-lf
		 :ontology-code-kr
		 ))|#

(dfc::defcomponent :om
    :use (:util :common-lisp)
    :nicknames (:ontologymanager)
    :system (
	     
	     :depends-on (:comm :logging :util :simplekb
				:transform-overhead :transform-code
				:ontology-manager-interface
			;;	:ontology-manager-overhead
				:ontology-code-lf
			;;	:ontology-code-kr
				)
	     :components ("macros"
			  "messages")
	     )
    )


(defun initialize-ontology (&key (kr t) (lf t))
  "loads the data into the ontologies *lf-ontology* and *kr-ontology*"      
  (when lf
    (setq *lf-ontology* (new-lf-ontology :default-lf-root 'ont::Root))      
    (mk:load-system :lfontology-data))    
  (when kr
    (setq *kr-ontology* (new-kr-ontology))
    (when lf
      (initialize-lf-kr-interface *lf-ontology* *kr-ontology* 'f::kr-type)
      )
      
    ;; NC
    (let ((type-hierarchy (new-type-hierarchy)))
      ;; add the base hierarchy to the LF
      (add-ling-ontology-to-hierarchy *lf-ontology* type-hierarchy)
      ;; load-new-scenario
      (load-scenario trips:*scenario*)
      ;; add the base hierarchy to the KR
      (add-kr-ontology-to-hierarchy *kr-ontology* type-hierarchy)
      )))

(defun recompile-ontology ()
  "when new types are added dynamically, we must recompile in order to enable subsumption"
  (let ((type-hierarchy (new-type-hierarchy)))
      ;; add the base hierarchy to the LF
      (add-ling-ontology-to-hierarchy *lf-ontology* type-hierarchy)))

(defmethod mk:operate-on-system :after ((name (eql :om)) (operation (eql :load)) &rest args)
    (declare (ignore args))
  (format *trace-output* "~&~%;;; om: initializing ontology~%~%")
  (initialize-ontology))

(defun load-scenario (scenario)
  "Loads the ontology additions for the given SCENARIO (name).
This is done by loading the defsys.lisp from the appropriate directory (if
it exists), then loading the :scenario-ontology system (if one is defined).
This function also calls LOAD-TRANSFORMS."
  (format *trace-output* "~&;; om: loading scenario: ~A~%" scenario)
  (let ((defsys-path (trips::make-trips-pathname
		      (format nil "src;OntologyManager;Data;Domains;~A;defsys.lisp" scenario))))
    (cond
      ((probe-file defsys-path)
       (load defsys-path)
       (if (mk::get-system :scenario-ontology)
	   (mk::load-system :scenario-ontology)
	   (format *error-output* "~&om: warning: no scenario-ontology system for scenario: ~A" scenario)))
      (t
       (format *error-output* "~&om: warning: no defsys for scenario: ~A~%" scenario))))
  ;; load the mapping transforms for LFMapper
  (load-transforms scenario)
   (format *trace-output* "~&;; om: done loading scenario: ~A~%" scenario))

(defun load-transforms (scenario)
  "Loads the LFMapper transforms for the given SCENARION (name).
This is done by loading the defsys.lisp from the appropriate directory (if
it exists), then loading the :lf-transforms system (if one is defined).
Note that this code unconditionally calls CLEAR-TRANSFORMS."
  ;; reset the current transforms
  ;; maybe not the right choice, but we will always reset them
  (clear-transforms)
  (format *trace-output* "~&;; om: loading transforms for scenario: ~A~%" scenario)
  (let ((defsys-path (trips::make-trips-pathname
		      (format nil "src;OntologyManager;Data;LFTransforms;~A;defsys.lisp" scenario))))
    (cond
      ((probe-file defsys-path)
       (load defsys-path)
       (if (mk::get-system :lf-transforms)
	   (mk::load-system :lf-transforms)
	   (format *error-output* "~&om: warning: no lf-transforms system for scenario: ~A" scenario)))
      (t
       (format *error-output* "~&om: warning: no LFMapper defsys for scenario: ~A~%" scenario)))))


(defun change-scenario (scenario)
  "Change to the given SCENARIO and re-initialize the ontology."
  (setq trips:*scenario* scenario)
  (initialize-ontology))

