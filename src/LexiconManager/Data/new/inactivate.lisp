;;;;
;;;; W::inactivate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::inactivate
     (wordfeats (W::morph (:forms (-vb) :nom w::inactivation)))
     (SENSES
      #|
     (
     (LF-PARENT ont::stop)
     (templ agent-effect-xp-templ)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
      |#
      (
       (LF-PARENT ont::stop)
      (templ agent-affected-xp-templ)
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::time-span F::atomic))
      )
    )
   )
))

