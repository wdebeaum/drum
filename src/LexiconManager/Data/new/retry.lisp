;;;;
;;;; W::retry
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::retry
   (SENSES
    ((LF-PARENT ONT::TRY-again)
     (example "retry this option")
     (meta-data :origin plow :entry-date 20050401 :change-date nil :comments nil)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-effect-xp-templ)
     )
    )
   )
))

