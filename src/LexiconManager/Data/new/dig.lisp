;;;;
;;;; W::dig
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::dig
   (wordfeats (W::morph (:forms (-vb) :past W::dug)))
   (SENSES
    ((LF-PARENT ONT::dig-scoop)
     (example "dig a hole")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::dig (W::out))
   (wordfeats (W::morph (:forms (-vb) :past W::dug)))
   (SENSES
    ((LF-PARENT ONT::cause-out-of)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "dig out the power lines")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::v
 :words (
  ((W::dig (W::through))
   (wordfeats (W::morph (:forms (-vb) :past W::dug)))
   (SENSES
    ((LF-PARENT ONT::physical-scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he dug through the papers")
     (TEMPL AGENT-NEUTRAL-XP-TEMPL)
     )
    )
   )
))
