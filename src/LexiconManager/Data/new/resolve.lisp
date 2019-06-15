;;;;
;;;; W::resolve
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::resolve
   (SENSES
    ((LF-PARENT ONT::SOLVE)
     (meta-data :origin task-learning :entry-date 20050830 :change-date nil :comments nil)
     (example "resolve the conflict")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
       )
   )
))

