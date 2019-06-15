;;;;
;;;; W::execute
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::execute
   (wordfeats (W::morph (:forms (-vb) :nom w::execution)))
   (SENSES
    ((LF-PARENT ONT::EXECUTE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL agent-affected-xp-templ)
     (TEMPL agent-neutral-xp-templ)
     (example "execute the action/plan")
     )
    )
   )
))

