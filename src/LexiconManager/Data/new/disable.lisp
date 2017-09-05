;;;;
;;;; W::disable
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::disable
   (SENSES
    ((EXAMPLE "disable all alerting")
     (LF-PARENT ont::disable)
     (templ agent-theme-xp-templ)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
     ((EXAMPLE "disable the alarm")
      (LF-PARENT ont::disable)
      (templ agent-affected-xp-templ)
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::time-span F::atomic))
      )
     ((meta-data :origin "wordnet-3.0" :entry-date 20090603 :change-date nil :comments nil)
     (LF-PARENT ONT::disable)
     (TEMPL agent-affected-effect-templ  (xp (% w::cp (w::ctype w::s-from-ing))))
     (example "he disabled the alarm from sounding")
     )
    )
   )
))

