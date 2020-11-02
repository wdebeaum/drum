;;;;
;;;; W::inactivate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::inactivate
     (wordfeats (W::morph (:forms (-vb) :nom w::inactivation)))
     (SENSES
      #|
     (
     (LF-PARENT ont::deactivate-turn-off) ;stop)
     (templ agent-effect-xp-templ)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
      |#
      (
       (LF-PARENT ont::deactivate-turn-off) ;stop)
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::time-span F::atomic))
      )
    )
   )
))

