;;;;
;;;; W::designate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::designate
   (SENSES
    ((EXAMPLE "designate the libraries")
     (LF-PARENT ONT::naming)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ)
     (meta-data :origin task-learning :entry-date 20050823 :change-date 20090501 :comments nil)
     )
    )
   )
))

