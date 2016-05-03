;;;;
;;;; W::Review
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::Review
   (wordfeats (W::morph (:forms (-vb) :nom W::review)))
   (SENSES
    ((LF-PARENT ONT::scrutiny)
     (example "review the plan")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     )
    )
   )
))

