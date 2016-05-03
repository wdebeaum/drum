;;;;
;;;; w::clip
;;;;

(define-words :pos w::N 
 :words (
;; Various electrical parts
  (w::clip
  (senses((LF-parent ONT::Device-component) 
	    (templ part-of-reln-templ)
	    (meta-data :origin bee :entry-date 20040407 :change-date 20040607 :comments (test-s portability-experiment))
	    ))
  ;; Myrosia added terminal and holder for portability experiment
)
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::clip
 (senses
  ((meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
   (LF-PARENT ONT::cut)
   (example "clip the polygon")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

