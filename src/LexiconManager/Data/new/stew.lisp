;;;;
;;;; w::stew
;;;;

(define-words :pos W::n
 :words (
  (w::stew
  (senses
	   ((LF-PARENT ONT::SOUP)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::stew
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::stew)
   (example "stew the chicken")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   )
  )
 )
))

