;;;;
;;;; W::urge
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
 (W::urge
   (SENSES
    ((LF-PARENT ONT::suggest)
     (example "Abrams urged Browne to hire Chiang")
     (meta-data :origin csli-ts :entry-date 20070313 :change-date nil :comments nil :wn nil)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-ADDRESSEE-THEME-OBJCONTROL-REQ-TEMPL)
     )
    ((LF-PARENT ONT::suggest)
     (example "Abrams urged Browne")
     (meta-data :origin csli-ts :entry-date 20070313 :change-date nil :comments nil :wn nil)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-ADDRESSEE-TEMPL)
     )
    )
   )
))

