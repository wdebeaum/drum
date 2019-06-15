;;;;
;;;; W::urge
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::urge
   (SENSES
    ((LF-PARENT ONT::suggest)
     (example "Abrams urged Browne to hire Chiang")
     (meta-data :origin csli-ts :entry-date 20070313 :change-date nil :comments nil :wn nil)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL)
     )
    ((LF-PARENT ONT::suggest)
     (example "Abrams urged Browne")
     (meta-data :origin csli-ts :entry-date 20070313 :change-date nil :comments nil :wn nil)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AGENT1-NP-TEMPL)
     )
    )
   )
))

