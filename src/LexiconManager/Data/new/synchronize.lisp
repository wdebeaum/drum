;;;;
;;;; W::synchronize
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::synchronize
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::coordinating)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-affected2-TEMPL (xp (% W::PP (W::ptype W::with))))
     )
    ;; need this template to go through the adjectival passive rule
    ((LF-PARENT ONT::coordinating)
     (example "synchronize the details")
     (TEMPL AGENT-affected-xp-TEMPL)
     )
    )
   )
))

