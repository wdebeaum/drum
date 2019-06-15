;;;;
;;;; w::seed
;;;;

(define-words :pos W::n
 :words (
  (w::seed
  (senses
	   ((LF-PARENT ONT::NUTS-SEEDS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::seed
 (senses
    
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::remove-from-food)
   (example "seed the tomatoes")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

