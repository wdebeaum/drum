;;;;
;;;; W::ice
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ice
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("ice%1:27:00"))
     (LF-PARENT ONT::substance)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((w::ice w::cream)
  (senses
	   ((LF-PARENT ONT::SWEETS)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::ICE W::TEA)
  (senses
	   ((LF-PARENT ONT::TEAS-COCKTAILS-BLENDS)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::ICE W::TEA)
  (senses
	   ((LF-PARENT ONT::TEAS-COCKTAILS-BLENDS)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
  ;; moving w::tea to ont::tea for CAET
)
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::ice
 (senses
  ((meta-data :origin foodkb :entry-date 20090129 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (syntax (w::resultative +))
   (example "he iced the cake")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-XP-TEMPL)
   )
  )
 )
))

