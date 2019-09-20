;;;;
;;;; W::LOCATE
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::LOCATE
   (SENSES
    ;;;;
    ;;;; disprefer this sense (only affects passive form)
    ((LF-PARENT ONT::find)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (PREFERENCE 0.96)
     )
    
    ((LF-PARENT ONT::put)
     (EXAMPLE "the company located their agents in LA")
     ;(SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL AGENT-AFFECTED-RESULT-XP-NP-TEMPL)
     (TEMPL AGENT-AFFECTED-RESULT-ADVBL-OBJCONTROL-TEMPL)
     )
    
    )
   )
))

