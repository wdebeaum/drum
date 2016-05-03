;; Nate Chambers
;;
;; LISP structures for holding transform rule definitions
;;

(in-package "ONTOLOGYMANAGER")


(defstruct lftransform
  name           ;; unique (hopefully) name identifier
  abstract       ;; t | nil
  default-rule   ;; t | nil
  typevar        ;; e.g. ?vv
  typetransform  ;; single typetransform structure
  argtransforms  ;; list of argtransform structures
  constraints    ;; list of constraints e.g. ((:obligatory :theme))
  defaults       ;; list of defaults e.g. ((?a LOAD) (?b take))
  functions
  )

(defstruct typetransform
  lftype   ;; e.g. ONT::MOVE
  lflex    ;; e.g. take
  kr       ;; e.g. LOAD
  )

(defstruct argtransform
;  lf       ;; e.g. NIL | (:theme ?a) | ((F ?a ont::TO-LOC :OF ?b) ...)
  content
  context
  kr       ;; e.g. (LOAD ?b ?a)  --> always a triple
  )

;; a report of a transform application: lf terms to which it applied, kr terms, and the transform applied
(defstruct transform-application-report
  lfterms name transform krterms)
