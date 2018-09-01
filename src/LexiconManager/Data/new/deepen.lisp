;;;;
;;;; w::deepen
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(w::deepen
 (senses
  ((meta-data :origin task-learning :entry-date 20050909 :change-date nil :comments nil)
   (LF-PARENT ONT::deepen)
   (example "they deepened the hole (to 9537 ft)")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (f::scale ont::depth-scale))
   (TEMPL AGENT-affected-xp-templ)
   )
   ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
    (EXAMPLE "The hole deepened as the workers continued to dig.")
    (LF-PARENT ONT::deepen)
    (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::depth-scale))
    (TEMPL affected-unaccusative-templ)
    )
  )
 )
))

