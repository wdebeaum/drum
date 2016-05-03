;;;;
;;;; W::require
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::require
   (SENSES
    ;;;; It requires cleaning
    ((LF-PARENT ONT::Necessity)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-theme-xp-templ)
     )
    )
   )
))

