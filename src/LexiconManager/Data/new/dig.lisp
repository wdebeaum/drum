;;;;
;;;; W::dig
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::dig
   (wordfeats (W::morph (:forms (-vb) :past W::dug)))
   (SENSES
    ((LF-PARENT ONT::dig)
     (example "dig a hole")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::dig (W::out))
   (wordfeats (W::morph (:forms (-vb) :past W::dug)))
   (SENSES
    ((LF-PARENT ONT::cause-out-of)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "dig out the power lines")
     (TEMPL AGENT-affected-xp-TEMPL)
     )
    )
   )
))

