;;;;
;;;; W::erase
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::erase
   (SENSES
    ((LF-PARENT ONT::CANCEL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected-XP-TEMPL)
     (EXAMPLE "erase the junk mail")
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :comments nil)
     )
    )
   )
))

