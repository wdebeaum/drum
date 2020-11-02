;;;;
;;;; W::deactivate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::deactivate
     (wordfeats (W::morph (:forms (-vb) :nom w::deactivation)))
   (SENSES
     ((EXAMPLE "deactivate all alerting") 
     (LF-PARENT ont::deactivate-turn-off) ;stop)
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
      ((EXAMPLE "deactivate the alarm")
       (LF-PARENT ont::deactivate-turn-off) ;stop)
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::time-span F::atomic))
      )
    )
   )
))

