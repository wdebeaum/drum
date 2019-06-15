;;;;
;;;; W::determine
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::determine
   (wordfeats (W::morph (:forms (-vb) :nom w::determination)))
   (SENSES
    ((LF-PARENT ONT::determine)
     (example "I determined that he left")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::determine)
     (example "determine the truck's capacity")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

