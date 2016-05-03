;;;;
;;;; defsys.lisp for OntologyManager/Data/Domains/default
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 21 May 2002
;;;; Time-stamp: <Mon Jan 18 12:04:13 EST 2010 ferguson>
;;;;
;;;; As of Jan 2010, the default scenario doesn't define any ontology
;;;; or lexicon additions. But this file shows how you would define them
;;;; if you needed them, and its presence also prevents the OM and LxM
;;;; from issuing warnings.
;;;;

(mk:defsystem :scenario-ontology
  :source-pathname #!TRIPS"src;OntologyManager;Data;Domains;default;"
  :components (
	       ;; "classes"
	       ;; "transforms"
	       ))

(mk:defsystem :scenario-lexicon
  :source-pathname #!TRIPS"src;OntologyManager;Data;Domains;default;"
  :components (
	       ;; "names"
	       ))
