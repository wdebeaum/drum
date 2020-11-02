;;;;
;;;; W::extinguish
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::extinguish
   (SENSES
      ((EXAMPLE "extinguish the candle")
       (LF-PARENT ont::deactivate-turn-off) ;stop)
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::time-span F::atomic))
      )
    )
   )
))

