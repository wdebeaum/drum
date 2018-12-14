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
   (LF-PARENT ONT::roast)
   (example "roast the chicken")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   )
   ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::roast)
   (example "the chicken roasted in the oven")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (f::time-span f::extended))
   (templ affected-templ)
   )
  )
 )
))

