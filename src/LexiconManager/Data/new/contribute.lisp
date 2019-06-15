;;;;
;;;; W::contribute
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::contribute
   (wordfeats (W::morph (:forms (-vb) :nom w::contribution)))
   (SENSES
    ((LF-PARENT ont::donate-give)
     (example "He contributed five dollars [to the donation]")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-goal-OPTIONAL-TEMPL)
     )
    )
   )
))

