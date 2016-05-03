;;;;
;;;; W::unload
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::unload
   (SENSES
    ;;;; swier -- unload the oranges from the truck
    ((LF-PARENT ONT::UNLOAD)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-SOURCE-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::off W::from)))))
     )
    ;;;; swier -- unload the truck
    ((LF-PARENT ONT::UNLOAD)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-SOURCE-TEMPL)
     )
    )
   )
))

