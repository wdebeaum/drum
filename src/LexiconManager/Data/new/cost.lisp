;;;;
;;;; W::COST
;;;;

(define-words :pos W::n
 :words (
  (W::COST
   (SENSES
    (
     ;(LF-PARENT ONT::value-COST)
     (LF-PARENT ONT::expense)
     (TEMPL OTHER-RELN-TEMPL)
     )
    (
     ;(LF-PARENT ONT::value-COST)
     (LF-PARENT ONT::expense)
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::COST
   (wordfeats (W::morph (:forms (-vb) :past W::cost)))
   (SENSES
    ((LF-PARENT ONT::COSTS)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-EXTENT-XP-TEMPL)
     (example "the truck costs five dollars")
     )
    ((LF-PARENT ONT::COSTS)
     (example "the truck cost him five dollars")
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-BENEFICIARY-EXTENT-XP-TEMPL)
     )
    ((LF-PARENT ONT::COSTS)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (example "it costs $5 to repair the truck") 
     (TEMPL EXPLETIVE-EXTENT-FORMAL-TEMPL)
     )
    ((LF-PARENT ONT::COSTS)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-EXTENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype w::s-to))))
     (example "the truck costs five dollars to repair")
     )
    )
   )
))

