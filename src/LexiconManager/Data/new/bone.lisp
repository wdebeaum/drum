;;;;
;;;; w::bone
;;;;

(define-words :pos W::n
 :words (
;; internal
  (w::bone
  (senses((LF-PARENT ONT::internal-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((W::BONE W::MARROW)
  (senses
	   ((LF-PARENT ONT::MEAT)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::bone
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (example "bone the chicken")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

