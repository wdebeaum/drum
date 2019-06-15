;;;;
;;;; W::clarify
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::clarify
   (wordfeats (W::morph (:forms (-vb) :nom W::clarification)))
   (SENSES
    ;;;; clarify the statement/the information
    ((LF-PARENT ONT::explain)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

