;;;;
;;;; W::operate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::operate
   (SENSES
    ((LF-PARENT ONT::FUNCTION)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-TEMPL)
     (EXAMPLE "you must ensure that the facility still operates")
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil)
     )
     ((LF-PARENT ONT::control-manage)
      (EXAMPLE "he operated the gun")
     (meta-data :origin step :entry-date 20080724 :change-date nil :comments step5)
     )
    )
   )
))

