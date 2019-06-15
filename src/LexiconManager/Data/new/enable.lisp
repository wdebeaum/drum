;;;;
;;;; W::enable
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::enable
   (SENSES
    ((EXAMPLE "enable all alerting")
     (LF-PARENT ont::enable)
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    ((EXAMPLE "enable the alarm [to sound]") 
     (LF-PARENT ont::enable)
     ;(TEMPL agent-affected-theme-objcontrol-optional-templ)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    )
   )
))

