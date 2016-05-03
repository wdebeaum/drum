;;;;
;;;; W::summarise
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::summarise
   (SENSES
    ;;;; summarize the information
    ((LF-PARENT ONT::SUMMARIZE) 
     (LF-FORM W::SUMMARISE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ)
     (meta-data :origin beetle :entry-date 20040607 :change-date nil :comments portability-experiment)
     )
    )
   )
))

