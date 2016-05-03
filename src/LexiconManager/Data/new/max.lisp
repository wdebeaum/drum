;;;;
;;;; W::max
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::max
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060413 :change-date nil :wn ("max%1:06:00") :comments nil)
     (LF-PARENT ONT::max-val)
     (TEMPL SUPERL-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
((w::max (w::out))
 (senses
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::maximize)
   (example "max out the budget")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-THEME-XP-TEMPL)
   )
  )
 )
))

