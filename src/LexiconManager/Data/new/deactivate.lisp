;;;;
;;;; W::deactivate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::deactivate
     (wordfeats (W::morph (:forms (-vb) :nom w::deactivation)))
   (SENSES
     ((EXAMPLE "deactivate all alerting") 
     (LF-PARENT ont::stop)
     (templ agent-effect-xp-templ)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
      ((EXAMPLE "deactivate the alarm")
       (LF-PARENT ont::stop)
      (templ agent-affected-xp-templ)
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::time-span F::atomic))
      )
    )
   )
))

