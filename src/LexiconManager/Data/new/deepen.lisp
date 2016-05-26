;;;;
;;;; w::deepen
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(w::deepen
 (senses
  ((meta-data :origin task-learning :entry-date 20050909 :change-date nil :comments nil)
   (LF-PARENT ONT::adjust)
   (example "deepen the indent (to 5 cm)")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-xp-templ)
   )
  )
 )
))

