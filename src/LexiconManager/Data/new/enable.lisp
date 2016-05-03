;;;;
;;;; W::enable
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::enable
   (SENSES
    ((EXAMPLE "enable all alerting")
     (LF-PARENT ont::enable)
     (templ agent-effect-xp-templ)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    ((EXAMPLE "enable the alarm [to sound]") 
     (LF-PARENT ont::enable)
;     (TEMPL agent-affected-theme-objcontrol-optional-templ)
     (TEMPL agent-EFFECT-AFFECTED-OBJCONTROL-TEMPL)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    )
   )
))

