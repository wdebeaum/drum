;;;;
;;;; W::solve
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::solve
   (SENSES
    ((LF-PARENT ONT::SOLVE)
     (example "solve the problem")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))

