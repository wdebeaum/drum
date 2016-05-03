;;;;
;;;; W::ROAST
;;;;

(define-words :pos W::n
 :words (
  (W::ROAST
  (senses
	   ((LF-PARENT ONT::MEAT)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::roast
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (example "roast the chicken")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
   ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (example "the chicken roasted in the oven")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
   (templ affected-templ)
   )
  )
 )
))

