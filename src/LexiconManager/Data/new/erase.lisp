;;;;
;;;; W::erase
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::erase
   (SENSES
    ((LF-PARENT ONT::DISCARD)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (EXAMPLE "erase the junk mail")
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :comments nil)
     )
    )
   )
))

