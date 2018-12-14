;;;;
;;;; W::SCALLOP
;;;;

(define-words :pos W::n
 :words (
  (W::SCALLOP
  (senses
	   ((LF-PARENT ONT::MOLLUSKS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::scallop
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::bake)
   (example "scallop the potatoes")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

