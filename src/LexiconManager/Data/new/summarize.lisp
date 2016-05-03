;;;;
;;;; W::summarize
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::summarize
   (SENSES
    ((LF-PARENT ONT::SUMMARIZE)
     (example "summarize the information")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))

