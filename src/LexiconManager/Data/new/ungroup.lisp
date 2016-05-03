;;;;
;;;; W::ungroup
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::ungroup
   (SENSES
    ((LF-PARENT ont::separation)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :comments nil)
     (EXAMPLE "ungroup the objects")
     )
    )
   )
))

