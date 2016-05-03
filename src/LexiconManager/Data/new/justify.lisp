;;;;
;;;; w::justify
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(w::justify
 (senses
  ((meta-data :origin task-learning :entry-date 20050824 :change-date nil :comments nil)
   (LF-PARENT ONT::arrange-text)
   (example "justify text within a text box")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-THEME-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  )
 )
))

