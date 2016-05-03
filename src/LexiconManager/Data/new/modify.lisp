;;;;
;;;; W::modify
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::modify
   (wordfeats (W::morph (:forms (-vb) :nom w::modification)))
   (SENSES
    ((LF-PARENT ONT::modify)
     (example "modify the plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

