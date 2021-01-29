;;;;
;;;; W::designate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::designate
   (SENSES
    ((EXAMPLE "designate the libraries")
     (LF-PARENT ONT::naming)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     ;(TEMPL AGENT-FORMAL-XP-TEMPL)
     (TEMPL AGENT-NEUTRAL-XP-TEMPL)
     (meta-data :origin task-learning :entry-date 20050823 :change-date 20090501 :comments nil)
     )
    )
   )
))

