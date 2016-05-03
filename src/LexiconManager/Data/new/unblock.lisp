;;;;
;;;; W::unBLOCK
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::unBLOCK
   (SENSES
    ((LF-PARENT ONT::START)
     (meta-data :origin calo-ontology :entry-date 20060523 :change-date nil :comments nil)
     (example "he unblocked the signal")
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-xp-templ) 
     )
    )
   )
))

