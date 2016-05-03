;;;;
;;;; W::predefine
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::predefine
   (SENSES
    ((LF-PARENT ONT::categorization)
     (meta-data :origin task-learning :entry-date 20050824 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "predefine this")
     (TEMPL agent-neutral-as-theme-optional-templ)
     )
    )
   )
))

