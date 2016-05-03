;;;;
;;;; W::lower
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::lower
      (wordfeats (W::morph (:forms (-vb) :past W::lowered :ing W::lowering)))
   (SENSES
    ((LF-PARENT ONT::MOVE-downward)
     (example "lower the banana a little")
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (meta-data :origin fruitcarts :entry-date 20050401 :change-date nil :comments fruitcart-07-2 :vn ("put_direction-9.4") :wn ("lower%2:38:00"))
     )
    )
   )
))

