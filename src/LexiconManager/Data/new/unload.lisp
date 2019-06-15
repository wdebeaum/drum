;;;;
;;;; W::unload
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::unload
   (SENSES
    ;;;; swier -- unload the oranges from the truck
    ((LF-PARENT ONT::UNLOAD)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::off W::from)))))
     )
    ;;;; swier -- unload the truck
    ((LF-PARENT ONT::UNLOAD)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-SOURCE-XP-TEMPL)
     )
    )
   )
))

