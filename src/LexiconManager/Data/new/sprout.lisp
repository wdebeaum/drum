;;;;
;;;; w::sprout
;;;;

(define-words :pos W::n
 :words (
  (w::sprout
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::sprout
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("sprout%2:30:00"))
     (templ agent-affected-xp-templ)
     (LF-PARENT ONT::life-transformation) ; like ferment
     )
    ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::life-transformation)
     (example "the seeds sprouted")
     (templ affected-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

