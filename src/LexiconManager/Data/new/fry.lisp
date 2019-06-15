;;;;
;;;; w::fry
;;;;

(define-words :pos W::n
 :words (
  (w::fry
  (senses
	   ((LF-PARENT ONT::FAST-FOOD)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::fry
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cook-in-fat)
   (example "fry an egg")
   (syntax (w::resultative +))
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
   )
  )
 )
))

