;;;;
;;;; W::revise
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::revise
     (wordfeats (W::morph (:forms (-vb) :nom w::revision)))
   (SENSES
    ((LF-PARENT ONT::REVISE)
     (example "revise the plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

