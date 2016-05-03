;;;;
;;;; W::COST
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::COST
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("cost%1:21:00"))
     (LF-PARENT ONT::value-COST)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::COST
   (wordfeats (W::morph (:forms (-vb) :past W::cost)))
   (SENSES
    ((LF-PARENT ONT::COSTS)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-cost-xp-templ)
     (example "the truck costs five dollars")
     )
    ((LF-PARENT ONT::COSTS)
     (example "the truck cost him five dollars")
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-BENEFICIARY-COST-templ)
     )
    ((LF-PARENT ONT::COSTS)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (example "it costs $5 to repair the truck") 
     (TEMPL theme-COST-EXPLETIVE-TEMPL)
     )
    ((LF-PARENT ONT::COSTS)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-cost-theme-templ (xp (% w::cp (w::ctype w::s-to))))
     (example "the truck costs five dollars to repair")
     )
    )
   )
))

